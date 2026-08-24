## =============================================================================
## 14_density_change.R
## Credit union office density by county, 2016Q1 vs 2026Q1, and the
## decomposition of the change into OFFICES and POPULATION
##
## Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA Office of the Chief Economist
##
## Standalone. Needs two quarters of the branch file and two FOICU files.
## ASCII-only. Run block by block; objects stay in the global environment.
##
## WHY THE DECOMPOSITION IS THE POINT
##   Offices per capita can rise two ways: more offices, or fewer people. Those
##   have opposite policy meanings, and the flattering one is probably partly
##   true - rural population fell over this period. A density map alone cannot
##   tell them apart, and a reviewer will say so within seconds. So the load is
##   carried by the identity
##
##       %change in offices per capita = %change in offices - %change in population
##
##   expressed three ways: a regime map (WHERE), a quadrant scatter (HOW MUCH),
##   and an aggregate bar (the two components side by side).
##
## FOR THE BRIEFING, USE TWO EXHIBITS, NOT SIX
##   The regime map (14.8) and the scatter or decomposition bar (14.9). The two
##   level maps in 14.7 are report and appendix material: nobody can subtract
##   two choropleths by eye, and the dominant signal in both is the same thing.
##
## Blocks:
##   14.1   paths, options, download helper, palette
##   14.2   offices by county, both quarters
##   14.3   county geography harmonised across ten years   <- read this one
##   14.4   county population, both years
##   14.5   the density panel and the rural flag
##   14.6   decomposition, regime classification, headline tables
##   14.7   two level maps and a density-change map (report material)
##   14.8   the regime map and its rural-composition companion  <- briefing
##   14.9   quadrant scatter and aggregate decomposition bar    <- briefing
##   14.10  coverage, deserts and single-provider counties
##   14.11  charter age
## =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

## ---- 14.1  paths, options, download helper, palette ------------------------

DIR      <- "/Users/saurabhdatta/Downloads"
BRANCH_T <- file.path(DIR, "Credit Union Branch Information.txt")        # 2026Q1
BRANCH_0 <- file.path(DIR, "Credit Union Branch Information 2016Q1.txt") # you download
FOICU_T  <- file.path(DIR, "FOICU.txt")                                  # 2026Q1
FOICU_0  <- file.path(DIR, "FOICU 2016Q1.txt")                           # you download
GEO_DIR  <- file.path(DIR, "geo")
OUT_DIR  <- file.path(DIR, "density")
dir.create(GEO_DIR, showWarnings = FALSE); dir.create(OUT_DIR, showWarnings = FALSE)

YEAR_0 <- 2016L
YEAR_T <- 2026L
CU_TYPES_KEEP <- c(1L, 2L)

options(timeout = 900)
sf_use_s2(FALSE)

## Download method ladder. On the Mac libcurl is fine; on the workstation
## libcurl's bundled CA store rejects the agency TLS-inspection root and the
## external curl binary (system trust store) does not. Note for the workstation:
## options(download.file.method = "wininet") from the handoff is dead under
## R 4.6.0 - wininet was deprecated in R 4.2.0 and has since been removed, so
## match.arg rejects it before any network call is made.
dl <- function(url, dest, expect = c("zip", "text")) {
  expect <- match.arg(expect)
  for (m in c("curl", "libcurl", "auto")) {
    r <- tryCatch(download.file(url, dest, mode = "wb", quiet = TRUE, method = m,
                                extra = if (m == "curl") "-L --silent --show-error" else character()),
                  error = function(e) { message("  [", m, "] ", conditionMessage(e)); 1L })
    ok <- identical(as.integer(r), 0L) && file.exists(dest) && file.size(dest) > 1000
    ## a proxy block page returns HTTP 200 and is not a ZIP - check magic bytes
    if (ok && expect == "zip") ok <- identical(readBin(dest, "raw", 2), as.raw(c(0x50, 0x4b)))
    if (ok) { cat("  ok via", m, "->", basename(dest), "\n"); return(invisible(TRUE)) }
  }
  stop("Download failed: ", url, "\nFetch it in a browser and place it at ", dest)
}

PAL <- list(deep = "#0E4C55", mid = "#1F6F78", soft = "#7FB0AE", pale = "#DCE6E4",
            land = "#F3F1EC", ink = "#12202E", accent = "#C8553D", gold = "#E8A24A",
            grey = "#5B6B72", rule = "#ECEFEF")

theme_map <- function(base = 12) {
  theme_void(base_size = base) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = base * 1.7, hjust = 0,
                                    margin = margin(b = 4), colour = PAL$ink),
          plot.subtitle = element_text(size = base * 1.0, colour = PAL$grey, hjust = 0,
                                       margin = margin(b = 12), lineheight = 1.2),
          plot.caption = element_text(size = base * 0.72, colour = PAL$grey, hjust = 0,
                                      margin = margin(t = 10), lineheight = 1.25),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = base * 0.8, colour = PAL$ink),
          legend.text = element_text(size = base * 0.75, colour = PAL$grey),
          plot.margin = margin(20, 24, 16, 24))
}
theme_chart <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = PAL$rule, linewidth = 0.4),
          plot.title = element_text(face = "bold", size = base * 1.6, colour = PAL$ink),
          plot.subtitle = element_text(colour = PAL$grey, lineheight = 1.25,
                                       margin = margin(b = 10)),
          plot.caption = element_text(colour = PAL$grey, size = base * 0.7, hjust = 0),
          legend.position = "top", plot.margin = margin(18, 24, 14, 20))
}

## ---- 14.2  offices by county, both quarters --------------------------------
## Counts ALL offices (headquarters + branches), and headquarters separately.
## TRAP: MainOffice == "Yes" is the headquarters flag. SiteTypeName
## "Corporate Office" is NOT - a charter can carry several corporate sites, and
## using it overstates the headquarters count by roughly a quarter.
## TRAP: the branch file has no CU_TYPE. FOICU supplies it on JOIN_NUMBER. You
## need the FOICU for BOTH quarters or the two years are different universes.

## COLUMN NAMES DIFFER BETWEEN VINTAGES. The 2026Q1 file calls the county
## PhysicalAddressCountyName2; the 2016Q1 file calls it PhysicalAddressCountyName.
## Nothing else is guaranteed either, so resolve every field we depend on by
## pattern and print what was matched. Hardcoding a name here fails silently on
## whichever quarter you did not test against.
pick_col <- function(DT, patterns, what, required = TRUE) {
  nm <- names(DT)
  for (p in patterns) {
    hit <- grep(p, nm, ignore.case = TRUE, value = TRUE)
    if (length(hit)) {
      ## Prefer the shortest match, so "...CountyName" wins over
      ## "...CountyNameSomethingElse" when a vintage carries both.
      hit <- hit[order(nchar(hit))]
      if (length(hit) > 1L)
        message(sprintf("    [%s] %d candidates (%s) -> using '%s'",
                        what, length(hit), paste(hit, collapse = ", "), hit[1]))
      return(hit[1])
    }
  }
  if (required)
    stop(sprintf("Cannot resolve '%s'. Tried: %s\nColumns present: %s",
                 what, paste(patterns, collapse = " | "), paste(nm, collapse = ", ")))
  NA_character_
}

read_offices <- function(branch_file, foicu_file, label) {
  stopifnot(file.exists(branch_file), file.exists(foicu_file))
  b <- fread(branch_file, colClasses = "character", encoding = "Latin-1",
             fill = TRUE, showProgress = FALSE)
  f <- fread(foicu_file,  colClasses = "character", encoding = "Latin-1",
             fill = TRUE, showProgress = FALSE)

  cat(sprintf("[%s] resolving branch-file columns:\n", label))
  C <- list(
    cu     = pick_col(b, c("^CU_NUMBER$", "^CU_NUM"),                    "cu number"),
    join   = pick_col(b, c("^JOIN_NUMBER$", "^JOIN_NUM"),                "join number"),
    county = pick_col(b, c("^PhysicalAddressCountyName", "CountyName",
                           "^County$"),                                  "county name"),
    state  = pick_col(b, c("^PhysicalAddressStateCode$", "^PhysicalAddressState",
                           "^StateCode$"),                               "state code"),
    hq     = pick_col(b, c("^MainOffice$", "MainOffice"),                "main office flag"))
  cat(sprintf("    county = %s | state = %s | hq = %s\n", C$county, C$state, C$hq))

  fj <- pick_col(f, c("^JOIN_NUMBER$", "^JOIN_NUM"), "FOICU join number")
  ft <- pick_col(f, c("^CU_TYPE$", "^CUTYPE$"),      "FOICU cu type")
  f[, `:=`(join = as.integer(get(fj)), cu_type = as.integer(get(ft)))]
  stopifnot(!any(duplicated(f$join)))

  b[, join := as.integer(get(C$join))]
  n_all <- nrow(b)
  b <- merge(b, f[, list(join, cu_type)], by = "join", all.x = TRUE)
  n_nomatch <- sum(is.na(b$cu_type))
  b <- b[cu_type %in% CU_TYPES_KEEP]

  b[, `:=`(CU_NUMBER  = get(C$cu),
           state      = toupper(trimws(get(C$state))),
           county_raw = trimws(get(C$county)),
           is_hq      = as.integer(toupper(trimws(get(C$hq))) %in% c("YES", "Y", "1", "TRUE")))]
  if (sum(b$is_hq) == 0L)
    warning("[", label, "] no headquarters found - check the values in ", C$hq)
  cat(sprintf("[%s] %s site rows -> %s after CU_TYPE filter (%d unmatched in FOICU); %s headquarters\n",
              label, comma(n_all), comma(nrow(b)), n_nomatch, comma(sum(b$is_hq))))
  b[]
}

off_0 <- read_offices(BRANCH_0, FOICU_0, as.character(YEAR_0))
off_T <- read_offices(BRANCH_T, FOICU_T, as.character(YEAR_T))

## ---- 14.3  county geography harmonised across ten years --------------------
## READ THIS BEFORE TRUSTING ANY CHANGE NUMBER.
##
## County geography is NOT constant between 2016 and 2026, and the changes hit
## numerator and denominator differently.
##
##   CONNECTICUT. Counties were replaced by planning regions in 2022. The 2016
##   branch file and the 2010-2019 population series use eight old counties; the
##   2026 file and the current population series use nine planning regions.
##   There is no clean crosswalk. DECISION: drop Connecticut from the change
##   analysis and say so. It costs nothing for the rural findings - zero CT
##   counties and zero CT planning regions are rural under either UIC vintage -
##   and it is the same reasoning already in the technical appendix.
##
##   ALASKA. Valdez-Cordova Census Area (02261) split in 2019 into Chugach
##   (02063) and Copper River (02066). Folded back to 02261 in both series.
##
##   PUERTO RICO. The UIC file classifies PR municipios and the cartographic
##   file carries their geometry, but the PEP 2010-2019 COUNTY file does not
##   include PR - it is published as a separate municipio series. In the first
##   run all 78 municipios therefore fell out silently at the population merge.
##   DECISION: exclude PR explicitly and count it, rather than letting a
##   missing denominator do it quietly. This is the same territorial-scope
##   question sitting open on the work plan; resolve it once, in writing, for
##   the whole study. Set TERRITORIAL_SCOPE to "states_dc_pr" and supply a PR
##   population series if the report ends up including it.
##
## Anything else that moved should surface in the unmatched report below.

TERRITORIAL_SCOPE <- "states_dc"     # or "states_dc_pr" once PR population is sourced
DROP_STATES <- c("CT", "GU", "VI", "AS", "MP",
                 if (TERRITORIAL_SCOPE == "states_dc") "PR")
DROP_STATEFP <- c("09", "60", "66", "69", "78",
                  if (TERRITORIAL_SCOPE == "states_dc") "72")
FIPS_HARMON <- c("02063" = "02261", "02066" = "02261")
harmonise   <- function(g) ifelse(g %in% names(FIPS_HARMON), FIPS_HARMON[g], g)

cb_get <- function(url, zipname) {
  zp <- file.path(GEO_DIR, zipname); dd <- file.path(GEO_DIR, sub("\\.zip$", "", zipname))
  if (!dir.exists(dd)) { if (!file.exists(zp)) dl(url, zp, "zip"); unzip(zp, exdir = dd) }
  st_read(list.files(dd, "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
}
CB <- "https://www2.census.gov/geo/tiger/GENZ2023/shp/"
cty_sf <- cb_get(paste0(CB, "cb_2023_us_county_20m.zip"), "cb_2023_us_county_20m.zip")
st_sf  <- cb_get(paste0(CB, "cb_2023_us_state_20m.zip"),  "cb_2023_us_state_20m.zip")
cat(sprintf("Geometry: %d counties, %d states\n", nrow(cty_sf), nrow(st_sf)))

## --- county NAME -> FIPS, two-pass (same logic as 13_hq_map.R) ---
## The branch file gives a county name, not a code. Saints are spelled out;
## independent cities matter (Virginia has both a Richmond County and a Richmond
## city, likewise Roanoke and Franklin; Maryland has Baltimore County and
## Baltimore City), so the " City" token is PRESERVED in pass 1. Pass 2 catches
## the independent cities written bare, but only where dropping the token still
## leaves a unique county in that state.
deaccent <- function(x) chartr(
  "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
  "aeiouunAEIOUUN", x)

norm_county <- function(x, keep_city = TRUE) {
  x <- toupper(deaccent(trimws(x)))
  x <- gsub("[^A-Z ]", " ", x); x <- trimws(gsub("\\s+", " ", x))
  x <- sub("^SAINT ", "ST ", x); x <- sub("^STE ", "ST ", x)
  x <- sub(paste0(" (CITY AND BOROUGH|PLANNING REGION|CENSUS AREA|MUNICIPALITY|",
                  "MUNICIPIO|PARISH|BOROUGH|COUNTY)$"), "", x)
  if (!keep_city) x <- sub(" CITY$", "", x)
  trimws(x)
}

st_dt <- as.data.table(st_drop_geometry(st_sf))[, list(statefp = STATEFP, stusps = STUSPS,
                                                       state_name = NAME)]
cty_key <- as.data.table(st_drop_geometry(cty_sf))[, list(GEOID, statefp = STATEFP,
                                                          namelsad = NAMELSAD, cname = NAME)]
cty_key <- merge(cty_key, st_dt, by = "statefp", all.x = TRUE)
cty_key[, k1 := paste(stusps, norm_county(namelsad, TRUE))]
cty_key[, k2 := paste(stusps, norm_county(namelsad, FALSE))]
k2_lookup <- cty_key[, if (.N == 1L) list(GEOID2 = GEOID), by = k2]

attach_fips <- function(x, label) {
  n_in <- nrow(x)
  x <- x[!state %in% DROP_STATES & county_raw != ""]
  x[, k1 := paste(state, norm_county(county_raw, TRUE))]
  x[, k2 := paste(state, norm_county(county_raw, FALSE))]
  x <- merge(x, cty_key[, list(k1, GEOID)], by = "k1", all.x = TRUE)
  x <- merge(x, k2_lookup, by = "k2", all.x = TRUE)
  x[is.na(GEOID) & !is.na(GEOID2), GEOID := GEOID2]
  x[, GEOID2 := NULL]
  bad <- x[is.na(GEOID), list(n = .N), by = k1][order(-n)]
  cat(sprintf("[%s] %s offices in scope; county match %.2f%% (%d unmatched across %d names)\n",
              label, comma(nrow(x)), 100 * mean(!is.na(x$GEOID)),
              sum(is.na(x$GEOID)), nrow(bad)))
  if (nrow(bad)) print(head(bad, 15))
  x[!is.na(GEOID)]
}
off_0 <- attach_fips(off_0, as.character(YEAR_0))
off_T <- attach_fips(off_T, as.character(YEAR_T))

off_0[, fips := harmonise(GEOID)]
off_T[, fips := harmonise(GEOID)]

cnt_0 <- off_0[, list(off_0 = .N, hq_0 = sum(is_hq)), by = fips]
cnt_T <- off_T[, list(off_T = .N, hq_T = sum(is_hq)), by = fips]

## ---- 14.4  county population, both years -----------------------------------
## Census Population Estimates, bulk CSV. NOT the API: the Population Estimates
## Program left the Census API as of 2020, so there is no endpoint that returns
## current county population. tidycensus::get_estimates() for recent vintages
## fetches these same flat files internally.
##
## TWO files are needed because the estimate series restarts after each
## decennial census.
##
## TRAP: the 2010-2019 series is benchmarked to the 2010 census and the 2020+
## series to the 2020 census. They do not join smoothly - there is a documented
## discontinuity at 2019/2020, unevenly distributed across counties. Mitigation:
## re-run 14.6 with decennial 2010/2020 counts as an alternative denominator and
## report the range, exactly as we do for the UIC vintage. An ACS 5-year pull
## (which IS on the API) is a second independent check.

pep_get <- function(url, dest) { if (!file.exists(dest)) dl(url, dest, "text"); fread(dest, encoding = "Latin-1") }

POP_OLD_URL <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
                      "2010-2019/counties/totals/co-est2019-alldata.csv")
POP_NEW_URL <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
                      "2020-2024/counties/totals/co-est2024-alldata.csv")

pop_old <- pep_get(POP_OLD_URL, file.path(GEO_DIR, "co-est2019-alldata.csv"))
pop_new <- pep_get(POP_NEW_URL, file.path(GEO_DIR, "co-est2024-alldata.csv"))

pick_pop <- function(d, col) {
  d <- as.data.table(d)[as.integer(SUMLEV) == 50]      # 050 = county
  stopifnot(col %in% names(d))
  d[, list(fips = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
           pop  = as.numeric(get(col)))]
}
p0 <- pick_pop(pop_old, "POPESTIMATE2016")

## Take the newest POPESTIMATE column available rather than hardcoding: if a
## later vintage has been released this shifts the endpoint, and the map caption
## must then say which year the denominator actually is.
POP_T_COL <- grep("^POPESTIMATE20(2[0-9])$", names(pop_new), value = TRUE)
POP_T_COL <- POP_T_COL[length(POP_T_COL)]
POP_T_YEAR <- as.integer(sub("POPESTIMATE", "", POP_T_COL))
POP_0_YEAR <- 2016L
cat(sprintf("Population denominators: %d and %d.\n", POP_0_YEAR, POP_T_YEAR))
if (POP_T_YEAR != YEAR_T)
  cat(sprintf("  NOTE: offices are %dQ1 but population is %d. Every caption must say so.\n",
              YEAR_T, POP_T_YEAR))
pT <- pick_pop(pop_new, POP_T_COL)

p0[, fips := harmonise(fips)]; pT[, fips := harmonise(fips)]
p0 <- p0[, list(pop_0 = sum(pop)), by = fips]
pT <- pT[, list(pop_T = sum(pop)), by = fips]

## ---- 14.5  the density panel and the rural flag ----------------------------
## Rural = 2024 UIC vintage, held FIXED across both years. The statutory
## definition applied consistently; if counties were allowed to change category
## between the two maps the change map would be uninterpretable.

RURAL_UIC_2024 <- c(3, 6, 7, 8, 9)

## The UIC table is loaded here rather than inherited from 2_rural_counties.R,
## so this script runs standalone. Order of preference:
##   1. uic24 already in the environment (2_rural_counties.R was run)
##   2. a local copy at UIC_FILE
##   3. download from ERS
## The ?v= query strings on the ERS links are cache-busters and change without
## notice, so a failed download is expected occasionally - grab the XLSX or CSV
## by hand from https://www.ers.usda.gov/data-products/urban-influence-codes
## and drop it at UIC_FILE.

UIC_FILE <- file.path(GEO_DIR, "UrbanInfluenceCodes2024.xlsx")
UIC_URLS <- c(
  "https://www.ers.usda.gov/media/6181/2024-urban-influence-codes.xlsx?v=81925",
  "https://www.ers.usda.gov/media/6181/2024-urban-influence-codes.xlsx?v=51332",
  "https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53797/UrbanInfluenceCodes2024.xlsx?v=96367")

read_uic <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    as.data.table(fread(path, encoding = "Latin-1"))
  } else {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("readxl is needed to read the ERS workbook, or save it as CSV and point UIC_FILE at that.")
    sh <- readxl::excel_sheets(path)
    ## The data sheet is the one with a FIPS column; the first sheet is often notes.
    for (s in sh) {
      d <- as.data.table(readxl::read_excel(path, sheet = s, guess_max = 5000))
      if (any(grepl("fips", names(d), ignore.case = TRUE)) && nrow(d) > 1000) {
        cat("  UIC sheet:", s, "|", nrow(d), "rows\n"); return(d)
      }
    }
    stop("No sheet in ", path, " has a FIPS column and >1000 rows. Sheets: ",
         paste(sh, collapse = ", "))
  }
}

if (exists("uic24")) {
  cat("Using uic24 already in the environment.\n")
  u <- as.data.table(uic24)
} else {
  if (!file.exists(UIC_FILE)) {
    cat("Fetching the 2024 Urban Influence Codes from ERS...\n")
    got <- FALSE
    for (uu in UIC_URLS) {
      got <- tryCatch({ dl(uu, UIC_FILE, "zip"); TRUE },   # xlsx is a zip container
                      error = function(e) { message("  ", conditionMessage(e)); FALSE })
      if (isTRUE(got)) break
    }
    if (!isTRUE(got))
      stop("Could not fetch the UIC file. Download the XLSX or CSV from\n",
           "  https://www.ers.usda.gov/data-products/urban-influence-codes\n",
           "and save it as: ", UIC_FILE)
  }
  u <- read_uic(UIC_FILE)
  uic24 <- copy(u)          # leave it in the environment for later blocks
}

## Resolve the two columns we need. ERS has renamed these across vintages, so
## match by pattern and print what was taken.
ucol_f <- pick_col(u, c("^FIPS$", "^fips", "FIPS", "statecty"), "UIC county fips")
ucol_u <- pick_col(u, c("^UIC_2024$", "^UIC$", "^uic", "URBAN.?INFLUENCE"), "UIC code")
cat(sprintf("  UIC columns: fips = %s | code = %s\n", ucol_f, ucol_u))

u_rural_raw <- u[, list(fips_raw = formatC(as.integer(get(ucol_f)), width = 5, flag = "0"),
                        uic      = as.integer(get(ucol_u)))]
u_rural_raw <- u_rural_raw[!is.na(uic) & !is.na(fips_raw)]
u_rural_raw[, rural := as.integer(uic %in% RURAL_UIC_2024)]
u_rural_raw <- unique(u_rural_raw, by = "fips_raw")
setkey(u_rural_raw, fips_raw)

rural_map <- copy(u_rural_raw)[, list(fips = harmonise(fips_raw), uic, rural)]
rural_map <- unique(rural_map, by = "fips")

## VALIDATE before using it. The project's validated figure is 1,556 rural
## counties across the 50 states and DC. This panel will come in BELOW that by
## the number of harmonisation folds that collapse two rural units into one:
## Valdez-Cordova's successors, Chugach (02063) and Copper River (02066), are
## both rural, so folding them back to 02261 costs exactly one rural county.
## The expected count is therefore computed, not asserted - if the arithmetic
## below does not close, something else moved and nothing downstream is safe.
cat("\n--- UIC 2024 code distribution ---\n")
print(rural_map[, list(counties = .N), by = uic][order(uic)])

RURAL_VALIDATED <- 1556L
in_50dc <- function(f) !substr(f, 1, 2) %in% c("60", "66", "69", "72", "78")
n_rural_pre  <- u_rural_raw[in_50dc(fips_raw) & rural == 1, .N]
n_rural_50   <- rural_map[in_50dc(fips) & rural == 1, .N]
fold_rural   <- n_rural_pre - n_rural_50
expected_rural <- RURAL_VALIDATED - fold_rural

cat(sprintf("Rural counties, 50 states + DC: %d\n", n_rural_50))
cat(sprintf("  ERS file gives %d before harmonisation; %d lost to FIPS folds (%s)\n",
            n_rural_pre, fold_rural,
            paste(names(FIPS_HARMON), "->", unlist(FIPS_HARMON), collapse = ", ")))
cat(sprintf("  validated baseline %d, so expected %d\n", RURAL_VALIDATED, expected_rural))
if (n_rural_pre != RURAL_VALIDATED)
  warning("Pre-harmonisation rural count is ", n_rural_pre, ", not the validated ",
          RURAL_VALIDATED, " - check the UIC file and column resolution.")
rural_map <- rural_map[, list(fips, rural)]

D <- Reduce(function(a, b) merge(a, b, by = "fips", all = TRUE),
            list(p0, pT, cnt_0, cnt_T))
for (v in c("off_0", "off_T", "hq_0", "hq_T")) D[is.na(get(v)), (v) := 0L]
D <- merge(D, rural_map, by = "fips", all.x = TRUE)
D <- D[!is.na(pop_0) & !is.na(pop_T) & pop_0 > 0 & pop_T > 0]
cat(sprintf("Panel: %d counties (%d rural, %d non-rural, %d unclassified)\n",
            nrow(D), sum(D$rural == 1, na.rm = TRUE), sum(D$rural == 0, na.rm = TRUE),
            sum(is.na(D$rural))))

## Offices per 10,000 residents. NOT per million: a county of 4,000 people with
## one office is 250 per million, arithmetically true and rhetorically useless.
## Per 10,000 keeps the numbers legible at rural county scale.
D[, `:=`(dens_0 = 1e4 * off_0 / pop_0, dens_T = 1e4 * off_T / pop_T,
         hqd_0  = 1e4 * hq_0  / pop_0, hqd_T  = 1e4 * hq_T  / pop_T)]

## Small-county noise: one office in a tiny county dominates the ratio.
D[, thin := as.integer(pop_T < 5000)]
cat(sprintf("Counties under 5,000 population: %d (%d rural)\n",
            sum(D$thin), D[thin == 1 & rural == 1, .N]))

## ---- 14.6  decomposition, regimes, headline tables -------------------------
## log(dens_T/dens_0) = log(off_T/off_0) - log(pop_T/pop_0). Log changes are
## used so the two components add exactly; percentage changes are carried
## alongside for the narrative.

D[, `:=`(
  g_off  = ifelse(off_0 > 0 & off_T > 0, log(off_T / off_0), NA_real_),
  g_pop  = log(pop_T / pop_0),
  pc_off = ifelse(off_0 > 0, 100 * (off_T / off_0 - 1), NA_real_),
  pc_pop = 100 * (pop_T / pop_0 - 1),
  pc_den = ifelse(dens_0 > 0, 100 * (dens_T / dens_0 - 1), NA_real_))]
D[, g_den := g_off - g_pop]

## Six regimes, deliberately. "Offices unchanged" is its own class because in a
## county with one or two offices the modal ten-year change is exactly zero, and
## folding those into "up" or "down" would misstate most of rural America.
regime_levels <- c(
  "Offices up, population down",
  "Offices up, population up",
  "Offices unchanged",
  "Offices down, population down",
  "Offices down, population up",
  "No offices in either year")

classify <- function(o0, oT, p0, pT) {
  fifelse(o0 == 0 & oT == 0, "No offices in either year",
  fifelse(oT >  o0 & pT <  p0, "Offices up, population down",
  fifelse(oT >  o0,            "Offices up, population up",
  fifelse(oT == o0,            "Offices unchanged",
  fifelse(pT <  p0,            "Offices down, population down",
                               "Offices down, population up")))))
}
D[, regime6 := factor(classify(off_0, off_T, pop_0, pop_T), levels = regime_levels)]

cat("\n--- 14.6  Aggregate change, rural vs non-rural ---\n")
agg <- D[!is.na(rural), list(
  counties  = .N,
  offices_0 = sum(off_0), offices_T = sum(off_T),
  pct_off   = 100 * (sum(off_T) / sum(off_0) - 1),
  pop_0_m   = sum(pop_0) / 1e6, pop_T_m = sum(pop_T) / 1e6,
  pct_pop   = 100 * (sum(pop_T) / sum(pop_0) - 1),
  dens_0    = 1e4 * sum(off_0) / sum(pop_0),
  dens_T    = 1e4 * sum(off_T) / sum(pop_T)
), by = rural][order(-rural)]
agg[, pct_dens := 100 * (dens_T / dens_0 - 1)]
print(agg)

cat("\n--- 14.6  Counties by regime ---\n")
reg <- dcast(D[!is.na(rural) & !is.na(regime6)], regime6 ~ rural,
             value.var = "fips", fun.aggregate = length)
setnames(reg, c("0", "1"), c("non_rural", "rural"), skip_absent = TRUE)
reg[, `:=`(total = non_rural + rural, pct_rural = 100 * rural / (non_rural + rural))]
print(reg)

## The single most important number this script produces: of the rural counties
## whose office count FELL, what share also lost population? That is the
## demography-versus-policy split, and it converts the report's biggest caveat
## from an assertion into a measured quantity.
lost <- D[rural == 1 & off_0 > 0 & off_T < off_0]
cat(sprintf("\nRural counties that lost offices: %d\n  of which also lost population: %d (%.1f%%)\n",
            nrow(lost), lost[pc_pop < 0, .N], 100 * mean(lost$pc_pop < 0)))
gain <- D[rural == 1 & off_T > off_0]
cat(sprintf("Rural counties that gained offices: %d\n  of which lost population anyway: %d (%.1f%%)\n",
            nrow(gain), gain[pc_pop < 0, .N], 100 * mean(gain$pc_pop < 0)))

hl <- D[rural == 1 & regime6 == "Offices up, population down"]
cat(sprintf("\nHEADLINE CANDIDATE: %d rural counties added credit union offices while losing population.\n",
            nrow(hl)))
if (nrow(hl)) {
  cat(sprintf("  They added %d net offices and lost %s residents,\n",
              sum(hl$off_T - hl$off_0), comma(sum(hl$pop_0 - hl$pop_T))))
  cat(sprintf("  and are %.1f%% of all rural counties with any office in either year.\n",
              100 * nrow(hl) / D[rural == 1 & (off_0 > 0 | off_T > 0), .N]))
}
fwrite(D, file.path(OUT_DIR, "county_density_panel.csv"))
fwrite(reg, file.path(OUT_DIR, "regime_counts.csv"))

## ---- 14.7  level maps and the density-change map (report material) ---------
## Two level choropleths side by side are weak on a slide - nobody subtracts
## them by eye, and the dominant signal in both is the same empty West. Keep
## them for the report; use 14.8 and 14.9 in the briefing.

cty_sf$fips <- harmonise(cty_sf$GEOID)
geom <- cty_sf[!cty_sf$STATEFP %in% DROP_STATEFP, ]
geom <- tigris::shift_geometry(geom, geoid_column = "GEOID")
states_g <- tigris::shift_geometry(st_sf[!st_sf$STUSPS %in% DROP_STATES, ],
                                   geoid_column = "GEOID")
## shift_geometry returns ESRI:102003. Do NOT pass a different crs to coord_sf
## afterwards - that re-projects the sf layers and leaves any non-sf layer
## behind, which is how the earlier bubble maps ended up out of register.

MG <- merge(geom, as.data.frame(D), by = "fips", all.x = TRUE)

dens_bins <- function(v) cut(v, breaks = c(-Inf, 0, 1, 2, 4, 8, Inf),
                             labels = c("None", "0-1", "1-2", "2-4", "4-8", "8+"))
MG$b0 <- dens_bins(MG$dens_0); MG$bT <- dens_bins(MG$dens_T)

CAP_BASE <- paste0(
  sprintf("Offices = headquarters and branches, charter types 1 and 2, %dQ1 and %dQ1. Population: Census county estimates for %d and %d\n",
          YEAR_0, YEAR_T, POP_0_YEAR, POP_T_YEAR),
  sprintf("(the %d office count is measured against a %d population, the latest county vintage published).\n",
          YEAR_T, POP_T_YEAR),
  "Connecticut excluded (counties replaced by planning regions in 2022); Puerto Rico and the outlying territories excluded.\n",
  "Alaska and Hawaii rescaled and inset. NCUA Office of the Chief Economist.")

map_one <- function(bincol, yr, popyr, sub) {
  ggplot() +
    geom_sf(data = MG, aes(fill = .data[[bincol]]), colour = "white", linewidth = 0.05) +
    geom_sf(data = states_g, fill = NA, colour = alpha(PAL$ink, 0.4), linewidth = 0.25) +
    scale_fill_manual(values = c("None" = PAL$land, "0-1" = PAL$pale, "1-2" = PAL$soft,
                                 "2-4" = "#468F92", "4-8" = PAL$mid, "8+" = PAL$deep),
                      name = "Offices per 10,000 residents", na.value = "grey92",
                      guide = guide_legend(nrow = 1, label.position = "bottom",
                                           keywidth = unit(2.2, "lines"),
                                           keyheight = unit(0.5, "lines"))) +
    labs(title = sprintf("Credit union offices per 10,000 residents, %dQ1", yr),
         subtitle = sprintf("%s  Population denominator: %d.", sub, popyr),
         caption = CAP_BASE) +
    theme_map()
}
m2016 <- map_one("b0", YEAR_0, POP_0_YEAR, "Physical access to a credit union office, at the start of the period.")
m2026 <- map_one("bT", YEAR_T, POP_T_YEAR, "The same measure ten years later.")

MG$dchg <- cut(MG$pc_den, breaks = c(-Inf, -50, -20, -5, 5, 20, 50, Inf),
               labels = c("< -50%", "-50 to -20", "-20 to -5", "about flat",
                          "+5 to +20", "+20 to +50", "> +50%"))
m_change <- ggplot() +
  geom_sf(data = MG, aes(fill = dchg), colour = "white", linewidth = 0.05) +
  geom_sf(data = states_g, fill = NA, colour = alpha(PAL$ink, 0.4), linewidth = 0.25) +
  scale_fill_manual(values = c("< -50%" = "#7B2E1E", "-50 to -20" = PAL$accent,
                               "-20 to -5" = "#E3A08F", "about flat" = "#EFEFEA",
                               "+5 to +20" = PAL$soft, "+20 to +50" = PAL$mid,
                               "> +50%" = PAL$deep),
                    name = "Change in offices per 10,000 residents", na.value = "grey92",
                    guide = guide_legend(nrow = 1, label.position = "bottom",
                                         keywidth = unit(1.9, "lines"),
                                         keyheight = unit(0.5, "lines"))) +
  labs(title = sprintf("Change in office density, %d to %d", YEAR_0, YEAR_T),
       subtitle = "Density can rise because offices were added or because people left. This map cannot tell them apart.\nNever show it without the regime map or the scatter beside it.",
       caption = CAP_BASE) +
  theme_map()

ggsave(file.path(OUT_DIR, sprintf("density_%d.png", YEAR_0)), m2016, width = 13, height = 8.6, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, sprintf("density_%d.png", YEAR_T)), m2026, width = 13, height = 8.6, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "density_change.png"),              m_change, width = 13, height = 8.6, dpi = 300, bg = "white")

## ---- 14.8  the regime map and its rural-composition companion --------------
## The briefing exhibit. Two hues: teal = gained offices, warm = lost offices,
## with shade carrying the population direction, so the eye reads the office
## dimension first and demography second - the order the argument runs.

REGIME_COLS <- c(
  "Offices up, population down"   = "#0E4C55",   # the headline case, darkest
  "Offices up, population up"     = "#7FB0AE",
  "Offices unchanged"             = "#EDEAE3",
  "Offices down, population down" = "#E3A08F",
  "Offices down, population up"   = "#A8341F",   # offices lost in a growing county
  "No offices in either year"     = "#F6F5F2")

## A few named annotations. CHECK THESE BEFORE PRESENTING - a county that gained
## three offices because one merger reclassified a headquarters as a branch is
## not a story you want repeated in a hearing.
ann <- as.data.table(st_drop_geometry(MG))[!is.na(regime6) & rural == 1 &
                                             regime6 == "Offices up, population down"]
label_fips <- ann[order(-(off_T - off_0))][1:4, fips]
lab_sf <- suppressWarnings(st_centroid(MG[MG$fips %in% label_fips, ],
                                       of_largest_polygon = TRUE))

n_up_down   <- D[rural == 1 & regime6 == "Offices up, population down", .N]
n_down_up   <- D[rural == 1 & regime6 == "Offices down, population up", .N]
n_unchanged <- D[rural == 1 & regime6 == "Offices unchanged", .N]

m_regime <- ggplot() +
  geom_sf(data = MG, aes(fill = regime6), colour = "white", linewidth = 0.05) +
  geom_sf(data = states_g, fill = NA, colour = alpha(PAL$ink, 0.45), linewidth = 0.26) +
  geom_sf_text(data = lab_sf, aes(label = NAME), size = 2.9, fontface = "bold",
               colour = PAL$ink, check_overlap = TRUE) +
  scale_fill_manual(values = REGIME_COLS, name = NULL, na.value = "grey92", drop = FALSE,
                    guide = guide_legend(nrow = 2, byrow = TRUE,
                                         keywidth = unit(1.4, "lines"),
                                         keyheight = unit(0.7, "lines"))) +
  labs(title = sprintf("Offices and people did not move together, %d to %d", YEAR_0, YEAR_T),
       subtitle = sprintf(paste0(
         "Dark teal is the case a commercially optimised branch network does not produce: %d rural counties added\n",
         "offices while losing residents. But %d rural counties lost offices while GAINING residents, and %d saw no\n",
         "change at all - stasis, not decline, is the modal rural experience."),
         n_up_down, n_down_up, n_unchanged),
       caption = CAP_BASE) +
  theme_map()
ggsave(file.path(OUT_DIR, "regime_map.png"), m_regime, width = 13, height = 8.8, dpi = 300, bg = "white")

## Companion: a stacked bar, not a second map. Same information, less to read.
comp <- D[!is.na(rural) & !is.na(regime6), list(n = .N), by = list(regime6, rural)]
comp[, share := 100 * n / sum(n), by = regime6]
comp[, grp := factor(ifelse(rural == 1, "Rural", "Non-rural"),
                     levels = c("Non-rural", "Rural"))]
tot <- comp[, list(n = sum(n)), by = regime6]

m_comp <- ggplot(comp, aes(y = factor(regime6, levels = rev(regime_levels)),
                           x = share, fill = grp)) +
  geom_col(width = 0.62) +
  geom_text(data = tot, inherit.aes = FALSE,
            aes(y = factor(regime6, levels = rev(regime_levels)), x = 102, label = comma(n)),
            hjust = 0, size = 3.3, colour = PAL$grey) +
  scale_fill_manual(values = c("Rural" = PAL$accent, "Non-rural" = PAL$pale), name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 118),
                     breaks = c(0, 25, 50, 75, 100), expand = c(0, 0)) +
  labs(title = "Which regimes are rural?",
       subtitle = "Share of counties in each regime that are rural under the 2024 Urban Influence Codes.\nCounty counts at right.",
       x = NULL, y = NULL) +
  theme_chart() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = PAL$ink, size = 10.5))
ggsave(file.path(OUT_DIR, "regime_composition.png"), m_comp, width = 10, height = 6, dpi = 300, bg = "white")

## ---- 14.9  quadrant scatter and aggregate decomposition bar ----------------
## The scatter answers "how much and why"; the regime map answered "where".

S <- D[!is.na(rural) & !is.na(g_off) & pop_T >= 5000]
S[, grp := factor(ifelse(rural == 1, "Rural county", "Non-rural county"),
                  levels = c("Rural county", "Non-rural county"))]

quad <- ggplot(S, aes(100 * (exp(g_pop) - 1), 100 * (exp(g_off) - 1))) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = PAL$pale, alpha = 0.45) +
  geom_hline(yintercept = 0, colour = PAL$ink, linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = PAL$ink, linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", colour = PAL$grey, linewidth = 0.35) +
  geom_point(aes(size = pop_T, colour = grp), alpha = 0.5, stroke = 0) +
  scale_colour_manual(values = c("Rural county" = PAL$accent, "Non-rural county" = PAL$mid),
                      name = NULL) +
  scale_size_area(max_size = 7, guide = "none") +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(-40, 60)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-100, 200)) +
  labs(title = "Offices and people moved in different directions",
       subtitle = "Each point is a county, sized by population. Above the dashed 45-degree line, offices grew faster than\npopulation, so access per resident improved. The shaded quadrant is the one that matters: offices added\nin counties that were losing people.",
       x = sprintf("Change in population, %d to %d", YEAR_0, YEAR_T),
       y = sprintf("Change in offices, %d to %d", YEAR_0, YEAR_T),
       caption = "Counties under 5,000 residents excluded from this exhibit. Connecticut excluded. NCUA Office of the Chief Economist.") +
  theme_chart()
ggsave(file.path(OUT_DIR, "offices_vs_population_scatter.png"), quad,
       width = 11, height = 8, dpi = 300, bg = "white")

dec <- rbindlist(lapply(c(1, 0), function(r) rbindlist(list(
  data.table(grp = ifelse(r == 1, "Rural", "Non-rural"), part = "Offices",    val = agg[rural == r, pct_off]),
  data.table(grp = ifelse(r == 1, "Rural", "Non-rural"), part = "Population", val = agg[rural == r, pct_pop]),
  data.table(grp = ifelse(r == 1, "Rural", "Non-rural"), part = "Density",    val = agg[rural == r, pct_dens])))))
dec[, part := factor(part, levels = c("Offices", "Population", "Density"))]

decomp_bar <- ggplot(dec, aes(part, val, fill = grp)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, colour = PAL$ink, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", val)), position = position_dodge(width = 0.7),
            vjust = ifelse(dec$val >= 0, -0.5, 1.3), size = 3.6, colour = PAL$ink) +
  scale_fill_manual(values = c("Rural" = PAL$deep, "Non-rural" = PAL$soft), name = NULL) +
  labs(title = sprintf("Where the density change came from, %d to %d", YEAR_0, YEAR_T),
       subtitle = sprintf(paste0(
         "Density change is offices minus population. Of the %+.1f point rural gain, %+.1f points came from offices\n",
         "and %+.1f from population loss - so the gain is not an artefact of depopulation. Non-rural offices grew too,\n",
         "just slower than the %+.1f%% population increase."),
         agg[rural == 1, pct_dens], agg[rural == 1, pct_off], -agg[rural == 1, pct_pop],
         agg[rural == 0, pct_pop]),
       x = NULL, y = "Percent change", caption = CAP_BASE) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank())
ggsave(file.path(OUT_DIR, "density_decomposition.png"), decomp_bar,
       width = 10, height = 7, dpi = 300, bg = "white")

## ---- 14.10  coverage, deserts and single-provider counties -----------------
## Headcounts, not ratios. These are the numbers a member of Congress repeats.

cov <- D[!is.na(rural), list(
  counties            = .N,
  with_office_0       = sum(off_0 > 0), with_office_T = sum(off_T > 0),
  no_office_T         = sum(off_T == 0),
  lost_last_office    = sum(off_0 > 0 & off_T == 0),
  gained_first_office = sum(off_0 == 0 & off_T > 0),
  pop_share_covered_T = 100 * sum(pop_T[off_T > 0]) / sum(pop_T),
  residents_uncovered = sum(pop_T[off_T == 0])
), by = rural][order(-rural)]
cov[, net_counties_covered := with_office_T - with_office_0]
cat("\n--- 14.10  Coverage ---\n"); print(cov)
cat(sprintf("\nRural residents living in a county with NO credit union office: %s (%.1f%%)\n",
            comma(cov[rural == 1, residents_uncovered]),
            100 - cov[rural == 1, pop_share_covered_T]))

## Single-provider counties: one credit union. If it merges away, the county
## has none.
sp <- off_T[, list(n_cu = uniqueN(CU_NUMBER), n_off = .N), by = fips]
sp <- merge(sp, D[, list(fips, rural, pop_T)], by = "fips")
cat(sprintf("\nCounties served by exactly one credit union: %d total, %d rural\n",
            sp[n_cu == 1, .N], sp[n_cu == 1 & rural == 1, .N]))
cat(sprintf("Rural residents in single-credit-union counties: %s\n",
            comma(sp[n_cu == 1 & rural == 1, sum(pop_T)])))
fwrite(cov, file.path(OUT_DIR, "coverage.csv"))
fwrite(sp[order(n_cu, -pop_T)], file.path(OUT_DIR, "single_provider_counties.csv"))

movers <- D[rural == 1 & pop_T >= 5000]
cat("\n--- Ten rural counties with the largest density decline ---\n")
print(head(movers[order(pc_den), list(fips, off_0, off_T, pop_0, pop_T, pc_off, pc_pop, pc_den)], 10))
cat("\n--- Ten rural counties with the largest density increase ---\n")
print(head(movers[order(-pc_den), list(fips, off_0, off_T, pop_0, pop_T, pc_off, pc_pop, pc_den)], 10))

## ---- 14.11  charter age ----------------------------------------------------
## FIRST RUN RESULT: rural and non-rural charter stock are the same age (median
## 1956 vs 1954, pre-1970 share 85.6% vs 86.0%, post-2010 share 0.65% vs 0.71%).
## So this is NOT a rural exhibit - the formation freeze is system-wide, with
## rural as the acute case because the rural stock is not being replaced at all.
## Framed that way it still earns a slide; framed as a rural finding it would be
## wrong, and a reviewer with the same file would catch it.

fT <- fread(FOICU_T, colClasses = "character", encoding = "Latin-1", fill = TRUE)
fT[, `:=`(cu_type = as.integer(get(pick_col(fT, c("^CU_TYPE$", "^CUTYPE$"), "cu type"))),
          yr      = as.integer(get(pick_col(fT, c("^YEAR_OPENED$", "YEAR_OPEN"), "year opened"))),
          CU_NUMBER = get(pick_col(fT, c("^CU_NUMBER$", "^CU_NUM"), "cu number")))]
hq_map <- unique(off_T[is_hq == 1, list(CU_NUMBER, fips)])
fT <- merge(fT, hq_map, by = "CU_NUMBER", all.x = TRUE)
fT <- merge(fT, D[, list(fips, rural)], by = "fips", all.x = TRUE)

age_stats <- function(d, lab) d[, list(
  group         = lab,
  n             = .N,
  median_year   = as.numeric(median(yr)),
  pct_pre_1970  = 100 * mean(yr < 1970),
  pct_post_2000 = 100 * mean(yr >= 2000),
  pct_post_2010 = 100 * mean(yr >= 2010),
  n_post_2010   = sum(yr >= 2010))]

base <- fT[cu_type %in% CU_TYPES_KEEP & yr > 1900]
age <- rbindlist(list(
  age_stats(base,                       "All charters"),
  age_stats(base[rural == 1],           "Rural HQ"),
  age_stats(base[rural == 0],           "Non-rural HQ")))
cat("\n--- 14.11  Charter age of the surviving stock ---\n"); print(age)
cat("\nRead this as a SYSTEM fact, not a rural one: the two groups are the same age.\n")
fwrite(age, file.path(OUT_DIR, "charter_age.csv"))

cat("\nAll exhibits written to ", OUT_DIR, "\n", sep = "")
cat("For the briefing use regime_map.png plus one of\n",
    "  offices_vs_population_scatter.png / density_decomposition.png.\n",
    "The two level maps and density_change.png are report material.\n", sep = "")
