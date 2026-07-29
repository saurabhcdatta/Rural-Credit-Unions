###############################################################################
# 5_branch_panel.R  -- stack 63 quarters and give every site a rural status
#
# Two things to keep straight:
#   * fill = TRUE is required. The 2013Q2 file has rows with 23 fields against
#     a 22-field header; without it fread stops early and silently returns a
#     third of the quarter.
#   * Rural is assigned on a FIXED 2024 vintage in every quarter, so a site
#     whose flag changes has actually MOVED. Vintage effects are measured
#     separately in 6_branch_series.R.
###############################################################################

## ---- 5.1 read one file first and look at it ---------------------------------
f1 <- list.files(BRANCH_DIR, "^branch_.*\\.txt$", full.names = TRUE)[1]
t1 <- fread(f1, colClasses = "character", fill = TRUE)
names(t1)                                                 ## LOOK
t1[1:3]                                                   ## LOOK
table(t1$SiteTypeName)                                    ## LOOK -- Branch / Corporate only
table(t1$MainOffice)                                      ## LOOK
head(sort(unique(t1$PhysicalAddressCountyName2)), 20)     ## LOOK -- names, not FIPS

## ---- 5.2 stack all 63 -------------------------------------------------------
files <- list.files(BRANCH_DIR, "^branch_.*\\.txt$", full.names = TRUE)
length(files)                                             ## LOOK -- 63

lst <- vector("list", length(files))
for (i in seq_along(files)) {
  x <- fread(files[i], colClasses = "character", fill = TRUE, showProgress = FALSE)
  setnames(x, tolower(gsub("[^A-Za-z0-9]+", "_", names(x))))
  cty <- grep("^physicaladdresscountyname", names(x), value = TRUE)[1]
  lst[[i]] <- x[, .(cu_number   = as.integer(cu_number),
                    site_id     = as.integer(siteid),
                    cyc         = as.Date(sub(" .*$", "", cycle_date), "%m/%d/%Y"),
                    site_type   = sitetypename,
                    main_office = toupper(substr(mainoffice, 1, 1)) == "Y",
                    city        = physicaladdresscity,
                    state       = physicaladdressstatecode,
                    zip5        = sprintf("%05d", as.integer(substr(physicaladdresspostalcode, 1, 5))),
                    county_name = trimws(get(cty)),
                    country     = physicaladdresscountry)]
  cat(basename(files[i]), ":", nrow(lst[[i]]), "sites\n")
}

b <- rbindlist(lst, fill = TRUE)
b[, `:=`(year = as.integer(format(cyc, "%Y")),
         q    = as.integer((as.integer(format(cyc, "%m")) - 1) %/% 3 + 1))]
b[, qidx := year * 4L + q]
b <- unique(b, by = c("cu_number", "site_id", "qidx"))
setorder(b, cu_number, site_id, qidx)

b[, .(site_quarters = .N, cus = uniqueN(cu_number),
      sites = uniqueN(paste(cu_number, site_id)), quarters = uniqueN(qidx))]   ## LOOK

## sites per quarter -- a quarter far below its neighbours is a PARSE FAILURE,
## not a real collapse
b[, .N, by = .(year, q)][order(year, q)]                  ## LOOK -- watch 2013Q2

## ---- 5.3 the county-name crosswalk ------------------------------------------
xw_raw <- setDT(haven::zap_labels(haven::read_dta(XWALK_FILE)))
setnames(xw_raw, tolower(gsub("[^A-Za-z0-9]+", "_", names(xw_raw))))
names(xw_raw)                                             ## LOOK
nm_cols <- grep("^countyname[0-9]*$", names(xw_raw), value = TRUE)
nm_cols                                                   ## LOOK -- countyname .. countyname5

## Resolve the FIPS column by pattern, not by literal name: this crosswalk
## calls it statecty_fips2, other builds call it statecty.
fp <- grep("^statecty", names(xw_raw), value = TRUE)[1]
fp                                                        ## LOOK -- which one matched
xw_raw[1:3, c("state", fp, nm_cols), with = FALSE]        ## LOOK -- the variants

xw_raw[, fips := sprintf("%05d", as.integer(get(fp)))]

## long: one row per (level, state, name key) -> fips
xw <- rbindlist(lapply(seq_along(nm_cols), function(i)
  data.table(level = i, variant = nm_cols[i],
             state = xw_raw$state, name = xw_raw[[nm_cols[i]]], fips = xw_raw$fips)))
xw <- xw[nzchar(trimws(name)) & !is.na(fips)]
xw[, key := city_key(name)]

## Refuse ambiguous keys. Stripping suffixes collapses Virginia's Franklin
## County and Franklin city into "franklin"; assigning either would be a guess
## about the treatment variable.
amb <- xw[, .(n = uniqueN(fips)), by = .(level, state, key)][n > 1L]
nrow(amb); head(amb, 10)                                  ## LOOK
xw <- unique(xw[!amb, on = .(level, state, key)], by = c("level", "state", "key"))
xw[, .N, by = .(level, variant)][order(level)]            ## LOOK

## ---- 5.4 match sites to counties, most specific variant first ---------------
b[, `:=`(fips = NA_character_, src = NA_character_,
         foreign = country != "United States" & nzchar(country),
         terr    = state %in% TERRITORIES,
         ckey    = city_key(county_name))]

b[!foreign & !terr & nzchar(county_name), .N]             ## LOOK -- candidates

for (lv in sort(unique(xw$level))) {
  idx <- b[, which(is.na(fips) & !foreign & !terr & nzchar(county_name))]
  if (!length(idx)) break
  got <- xw[level == lv][b[idx, .(state, k = ckey)], on = .(state, key = k), x.fips]
  b[idx, fips := got]
  b[idx[!is.na(got)], src := paste0("variant ", lv)]
  cat("variant", lv, ":", sum(!is.na(got)), "\n")
}

## Connecticut planning regions -- NCUA abbreviates them ("NW Hills",
## "Naugatuck Vly"), so match to the ERS names by keyword.
ct_pat <- c("capitol"="capitol", "greater bridgeport"="greater bridgeport",
            "lower ct river vly"="lower connecticut river", "naugatuck vly"="naugatuck",
            "northeastern ct"="northeastern", "nw hills"="northwest hills",
            "south central ct"="south central", "southeastern ct"="southeastern",
            "western ct"="western")
ct_ref <- u24[state == "CT"]
ct_map <- rbindlist(lapply(names(ct_pat), function(k) {
  h <- ct_ref[grepl(ct_pat[[k]], tolower(county_name))]
  if (nrow(h) != 1L) return(NULL)
  data.table(nm = k, fips = h$fips, ers = h$county_name)
}))
ct_map                                                    ## LOOK -- 9 rows, 09110-09190

idx <- b[, which(is.na(fips) & state == "CT")]
got <- ct_map$fips[match(norm_nm(b$county_name[idx]), ct_map$nm)]
b[idx, fips := got]; b[idx[!is.na(got)], src := "CT planning region"]
sum(!is.na(got))                                          ## LOOK

## ---- 5.5 recover the rest ---------------------------------------------------
## (a) within-site carry: SiteId is stable, so a blank county in one quarter
##     can be filled from the same site's own report in an adjacent quarter.
##     Recovery, not imputation. Nearest-in-time, so relocations survive.
setorder(b, cu_number, site_id, qidx)
b[, .f := fips]
b[, .f := { i2 <- which(!is.na(.f))
            if (!length(i2)) .f else { p <- findInterval(seq_along(.f), i2); p[p == 0L] <- 1L; .f[i2[p]] } },
  by = .(cu_number, site_id)]
hit <- b[, which(is.na(fips) & !foreign & !terr & !is.na(.f))]
b[hit, `:=`(fips = .f, src = "within-site carry")]
b[, .f := NULL]
length(hit)                                               ## LOOK

## (b) ZIP fallback, only where every county the ZCTA touches agrees on rural
zcta_file <- file.path(CACHE_DIR, "tab20_zcta520_county20_natl.txt")
if (!file.exists(zcta_file))
  download.file("https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt",
                zcta_file, mode = "wb")

z <- fread(zcta_file, sep = "|", colClasses = "character", showProgress = FALSE)
names(z)                                                  ## LOOK
xz <- z[nzchar(GEOID_ZCTA5_20) & nzchar(GEOID_COUNTY_20),
        .(zip5 = GEOID_ZCTA5_20, fips = GEOID_COUNTY_20)]
xz <- unique(merge(xz, u24[, .(fips, rural)], by = "fips"))
zok <- xz[, .(n = uniqueN(rural), fips = fips[1]), by = zip5][n == 1L]
xz[, uniqueN(rural), by = zip5][V1 > 1L, .N]              ## LOOK -- refused as ambiguous

idx <- b[, which(is.na(fips) & !foreign & !terr)]
got <- zok$fips[match(b$zip5[idx], zok$zip5)]
b[idx, fips := got]; b[idx[!is.na(got)], src := "ZIP crosswalk"]
sum(!is.na(got))                                          ## LOOK

## ---- 5.6 historical FIPS forward, then attach rural -------------------------
b[FIPS_FIX, on = .(fips = old), fips := i.new]

b[u24[, .(fips, r = rural)], on = "fips", rural_site := i.r]
b[u13[, .(fips, r = rural)], on = "fips", rural_site_13 := i.r]

## pre-2022 CT counties: not in the 2024 UIC, but no CT geography is rural
b[state == "CT" & is.na(rural_site),    rural_site    := 0L]
b[state == "CT" & is.na(rural_site_13), rural_site_13 := 0L]

b[, .N, by = src][order(-N)]                              ## LOOK -- match by source
b[!foreign & !terr, .(unmatched = sum(is.na(rural_site)),
                      pct = round(100 * mean(is.na(rural_site)), 2))]   ## LOOK
b[!foreign & !terr & is.na(rural_site), .N, by = .(state, county_name)][order(-N)][1:15]  ## LOOK
b[, .(foreign = sum(foreign), territory = sum(terr))]     ## LOOK -- excluded by design

b[, ckey := NULL]
saveRDS(b, file.path(DATA_DIR, "_branch_panel.rds"))

cat("\nBranch panel ready as `b`. Next: 6_branch_series.R\n")
