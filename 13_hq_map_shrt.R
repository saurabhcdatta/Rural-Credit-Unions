## =============================================================================
## 13_hq_map.R
## Credit union HEADQUARTERS on a US map -- presentation graphics
##
## Rural Credit Unions Study | NCUA Office of the Chief Economist
## Source: Credit_Union_Branch_Information.txt (2026Q1, 22,829 site records)
##
## Standalone -- does not require the numbered pipeline.
## ASCII-only by design; no smart quotes, en dashes or accented characters.
##
## Blocks:
##   13.1  paths, download method, palette
##   13.2  read the branch file, isolate headquarters
##   13.3  county and state geometry
##   13.4  county name harmonisation, two-pass join, reconciliation
##   13.5  map A -- county choropleth
##   13.6  map B -- bubbles
##   13.7  map C -- dark variant for slides
##   13.8  write PNGs and backing tables
##
## THREE THINGS THAT WERE WRONG EARLIER AND ARE FIXED HERE
##
## 1. options(download.file.method = "wininet") does not work under R 4.6.0.
##    wininet was deprecated in R 4.2.0 and has since been removed, so
##    match.arg rejects it before any network call happens. Scripts
##    2_rural_counties.R and 4_branch_download.R carry the same stale setting
##    from the handoff and will fail identically. See 13.1.
##
## 2. tigris::shift_geometry() returns ESRI:102003, NOT EPSG:5070. Passing
##    coord_sf(crs = 5070) re-projected the sf layers (a ~1,500 km shift in Y,
##    the two CRSs differ in latitude of origin) while leaving geom_point's
##    raw X/Y in 102003 -- bubbles rendered as a correctly shaped cloud sitting
##    well below their own basemap. Fixed by dropping coord_sf(crs = ...) and
##    drawing the bubbles as an sf layer so they cannot desync. See 13.6.
##
## 3. Connecticut. Census replaced CT counties with planning regions in 2022
##    and the cb_2023 county file carries the regions; NCUA writes them
##    abbreviated ("Naugatuck Vly", "NW Hills"). 64 CT headquarters failed the
##    join silently. See CT_ALIAS in 13.4. This affects the MAP only -- no CT
##    county and no CT planning region is rural under either UIC vintage, so
##    no rural finding in the study is touched.
## =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

## ---- 13.1  paths, download method, palette ---------------------------------

BRANCH_FILE <- "/Users/saurabhdatta/Downloads/Credit Union Branch Information.txt"
GEO_DIR     <- "/Users/saurabhdatta/Downloads"
MAP_DIR     <- "/Users/saurabhdatta/Downloads"
dir.create(GEO_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MAP_DIR, recursive = TRUE, showWarnings = FALSE)

options(timeout = 600)

## The external curl.exe Windows ships in System32 is Schannel-backed and reads
## the Windows certificate store, so it trusts the agency TLS-inspection root.
## libcurl carries its own CA bundle and does not. Confirm the backend before
## relying on it: if PATH resolves to a Git-for-Windows or Rtools curl, that
## one is OpenSSL-based and fails exactly like libcurl.
CURL_BIN <- Sys.which("curl")
cat("curl binary:", if (nzchar(CURL_BIN)) CURL_BIN else "NOT FOUND", "\n")
if (nzchar(CURL_BIN)) system2(CURL_BIN, "-V")
cat("^ look for 'Schannel' in the line above.\n\n")

## A proxy block page returns HTTP 200 and is not a ZIP, so validate magic
## bytes (50 4b) rather than trusting the status code.
dl <- function(url, dest) {
  for (m in c("curl", "libcurl", "auto")) {
    r <- tryCatch(
      download.file(url, dest, mode = "wb", quiet = TRUE, method = m,
                    extra = if (m == "curl") "-L --silent --show-error" else character()),
      error = function(e) { message("  [", m, "] ", conditionMessage(e)); 1L })
    if (identical(as.integer(r), 0L) && file.exists(dest) &&
        identical(readBin(dest, "raw", 2), as.raw(c(0x50, 0x4b)))) {
      cat("  downloaded via method =", m, "\n")
      return(invisible(TRUE))
    }
  }
  stop("All download methods failed for:\n  ", url,
       "\nFetch it in a browser (which uses the Windows certificate store)",
       "\nand drop the ZIP in ", normalizePath(GEO_DIR, mustWork = FALSE))
}

sf_use_s2(FALSE)

PAL <- list(
  ink   = "#12202E",
  paper = "#FBFAF7",
  land  = "#F3F1EC",
  seq   = c("#F3F1EC", "#DCE6E4", "#B7D0CE", "#7FB0AE", "#468F92", "#1F6F78", "#0E4C55")
)

theme_map <- function(base_size = 12, dark = FALSE) {
  bg <- if (dark) PAL$ink else PAL$paper
  fg <- if (dark) "#F2EFE9" else PAL$ink
  theme_void(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = bg, colour = NA),
      panel.background  = element_rect(fill = bg, colour = NA),
      legend.background = element_rect(fill = bg, colour = NA),
      plot.title    = element_text(colour = fg, face = "bold", size = base_size * 1.9,
                                   hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = adjustcolor(fg, 0.75), size = base_size * 1.05,
                                   hjust = 0, margin = margin(b = 14), lineheight = 1.25),
      plot.caption  = element_text(colour = adjustcolor(fg, 0.55), size = base_size * 0.78,
                                   hjust = 0, margin = margin(t = 14), lineheight = 1.3),
      legend.title  = element_text(colour = fg, face = "bold", size = base_size * 0.85),
      legend.text   = element_text(colour = adjustcolor(fg, 0.8), size = base_size * 0.8),
      legend.position = "bottom",
      legend.margin = margin(t = 6),
      plot.margin = margin(22, 26, 18, 26)
    )
}

## ---- 13.2  read and isolate headquarters -----------------------------------
## TRAP: SiteTypeName is NOT the headquarters flag. The 2026Q1 file has 5,394
## sites labelled "Corporate Office" but only 4,329 credit unions -- some
## charters carry several corporate sites. MainOffice == "Yes" is exactly one
## per CU_NUMBER. Using SiteTypeName overstates the HQ count by about 25%.

raw <- fread(BRANCH_FILE, colClasses = "character", encoding = "Latin-1",
             fill = TRUE, showProgress = FALSE)
cat(sprintf("Read %s site records, %d columns\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

hq <- raw[MainOffice == "Yes"]
stopifnot(uniqueN(hq$CU_NUMBER) == nrow(hq))
N_HQ_ALL <- nrow(hq)
cat(sprintf("Headquarters: %s (one per charter)\n", format(N_HQ_ALL, big.mark = ",")))

hq[, `:=`(state      = toupper(trimws(PhysicalAddressStateCode)),
          county_raw = trimws(PhysicalAddressCountyName2))]

## The 20m cartographic file covers the 50 states, DC and Puerto Rico. Guam and
## the US Virgin Islands have no geometry at this scale -- exclude, count, and
## state it on the map rather than dropping silently.
TERR_NO_GEOM <- c("GU", "VI", "AS", "MP")
n_terr     <- hq[state %in% TERR_NO_GEOM, .N]
n_intl     <- hq[PhysicalAddressCountry != "United States", .N]
n_nocounty <- hq[!state %in% TERR_NO_GEOM & PhysicalAddressCountry == "United States" &
                 (county_raw == "" | is.na(county_raw)), .N]
cat(sprintf("Excluded: %d island-territory HQs, %d non-US, %d missing county name\n",
            n_terr, n_intl, n_nocounty))

hq_us <- hq[PhysicalAddressCountry == "United States" &
            !state %in% TERR_NO_GEOM & county_raw != ""]
N_HQ_MAPPABLE <- nrow(hq_us)   # every one of these must land on a county

## ---- 13.3  geometry --------------------------------------------------------
## Census cartographic boundary files, fetched once and cached. Deliberately
## NOT tigris::counties() -- tigris downloads through httr/libcurl and hits the
## same CA-bundle wall. tigris is used only for shift_geometry(), which is a
## pure geometry operation requiring no network.

cb_get <- function(url, zipname) {
  zp <- file.path(GEO_DIR, zipname)
  dd <- file.path(GEO_DIR, sub("\\.zip$", "", zipname))
  if (!dir.exists(dd)) {
    if (!file.exists(zp)) dl(url, zp)
    if (!identical(readBin(zp, "raw", 2), as.raw(c(0x50, 0x4b))))
      stop("Not a ZIP -- likely a proxy block page: ", zp)
    unzip(zp, exdir = dd)
  }
  st_read(list.files(dd, "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
}

BASE <- "https://www2.census.gov/geo/tiger/GENZ2023/shp/"
cty_sf <- cb_get(paste0(BASE, "cb_2023_us_county_20m.zip"), "cb_2023_us_county_20m.zip")
st_sf  <- cb_get(paste0(BASE, "cb_2023_us_state_20m.zip"),  "cb_2023_us_state_20m.zip")
cat(sprintf("Geometry: %d counties, %d states\n", nrow(cty_sf), nrow(st_sf)))

## Alaska at true position sets the national scale on its own; Hawaii and
## Puerto Rico sit off the frame. shift_geometry() rescales and insets them,
## and returns ESRI:102003. Do NOT then pass a different crs to coord_sf --
## see the header note. Every layer below stays in whatever this returns.
cty_sf <- tigris::shift_geometry(cty_sf, geoid_column = "GEOID")
st_sf  <- tigris::shift_geometry(st_sf,  geoid_column = "GEOID")
cat("Working CRS after shift_geometry: ", st_crs(cty_sf)$input, "\n", sep = "")
stopifnot(st_crs(cty_sf) == st_crs(st_sf))

## Postal codes come off the state file; the county CB layer does not carry
## STUSPS in every vintage.
st_dt <- as.data.table(st_drop_geometry(st_sf))[, list(statefp = STATEFP,
                                                       stusps  = STUSPS,
                                                       state_name = NAME)]

## ---- 13.4  county name harmonisation, two-pass join ------------------------
## The part that decides whether the map is right. The branch file gives a
## county NAME, not a FIPS code:
##
##   - saints spelled out:  "Saint Tammany" vs Census "St. Tammany Parish"
##   - independent cities:  "Richmond City" vs Census "Richmond city", and
##     Virginia has BOTH a Richmond County and a Richmond city -- likewise
##     Roanoke, Franklin and Fairfax. Maryland has Baltimore County and
##     Baltimore City; Missouri has St. Louis County and St. Louis City. So
##     the " City" token must be PRESERVED. Stripping it collides two real
##     counties and silently doubles one of them.
##   - but some independent cities arrive WITHOUT the suffix: VA "Radford"
##     and "Salem" are cities, written bare. Those need the second pass.
##   - Connecticut planning regions, abbreviated by NCUA (see CT_ALIAS).
##   - Louisiana parishes, Alaska boroughs and census areas, Puerto Rico
##     municipios, accented names (Dona Ana NM, Bayamon PR).
##
## Census NAMELSAD is used, not NAME, because NAME alone does not distinguish
## "Richmond" the county from "Richmond" the city.

## Explicit transliteration. iconv //TRANSLIT is platform-dependent on Windows
## and can emit "?" where an accent was, which the character-class strip below
## would then turn into a space -- producing a plausible-looking no-match.
deaccent <- function(x) {
  from <- "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1"
  to   <- "aeiouunAEIOUUN"
  chartr(from, to, x)
}

norm_county <- function(x, keep_city = TRUE) {
  x <- toupper(deaccent(trimws(x)))
  x <- gsub("[^A-Z ]", " ", x)          # periods, apostrophes, hyphens -> space
  x <- trimws(gsub("\\s+", " ", x))
  x <- sub("^SAINT ", "ST ", x)
  x <- sub("^STE ",   "ST ", x)         # Ste. Genevieve MO
  ## longest suffix first: "CITY AND BOROUGH" must win over "BOROUGH"
  x <- sub(paste0(" (CITY AND BOROUGH|PLANNING REGION|CENSUS AREA|MUNICIPALITY|",
                  "MUNICIPIO|PARISH|BOROUGH|COUNTY)$"), "", x)
  if (!keep_city) x <- sub(" CITY$", "", x)
  trimws(x)
}

## Connecticut: Census carries planning regions, NCUA abbreviates them.
CT_ALIAS <- c(
  "CAPITOL"            = "CAPITOL",
  "SOUTH CENTRAL CT"   = "SOUTH CENTRAL CONNECTICUT",
  "GREATER BRIDGEPORT" = "GREATER BRIDGEPORT",
  "WESTERN CT"         = "WESTERN CONNECTICUT",
  "NAUGATUCK VLY"      = "NAUGATUCK VALLEY",
  "SOUTHEASTERN CT"    = "SOUTHEASTERN CONNECTICUT",
  "NW HILLS"           = "NORTHWEST HILLS",
  "LOWER CT RIVER VLY" = "LOWER CONNECTICUT RIVER VALLEY",
  "NORTHEASTERN CT"    = "NORTHEASTERN CONNECTICUT")

cty_key <- as.data.table(st_drop_geometry(cty_sf))[
  , list(GEOID, statefp = STATEFP, namelsad = NAMELSAD)]
cty_key <- merge(cty_key, st_dt, by = "statefp", all.x = TRUE)
cty_key[, k1 := paste(stusps, norm_county(namelsad, keep_city = TRUE))]
cty_key[, k2 := paste(stusps, norm_county(namelsad, keep_city = FALSE))]

## Pass 2 is only safe where dropping " City" still leaves a unique county in
## that state. Roanoke, Baltimore and St. Louis are ambiguous under k2 and are
## excluded from it -- they will have matched on k1 anyway.
k2_lookup <- cty_key[, if (.N == 1L) list(GEOID2 = GEOID), by = k2]

hq_us[, k1 := paste(state, norm_county(county_raw, keep_city = TRUE))]
hq_us[, k2 := paste(state, norm_county(county_raw, keep_city = FALSE))]

## CT override. Unrecognised CT names keep their original key so they surface
## in the unmatched table instead of turning into a silent "CT NA".
hq_us[state == "CT", ct_hit := CT_ALIAS[norm_county(county_raw)]]
hq_us[state == "CT" & !is.na(ct_hit), `:=`(k1 = paste("CT", ct_hit),
                                           k2 = paste("CT", ct_hit))]
hq_us[, ct_hit := NULL]

hq_cty <- hq_us[, list(n_hq = .N,
                       cu_names = paste(head(sort(CU_NAME), 3), collapse = " | ")),
                by = list(k1, k2)]

hq_cty <- merge(hq_cty, cty_key[, list(k1, GEOID)], by = "k1", all.x = TRUE)
n_pass1 <- hq_cty[!is.na(GEOID), sum(n_hq)]
hq_cty  <- merge(hq_cty, k2_lookup, by = "k2", all.x = TRUE)
hq_cty[is.na(GEOID) & !is.na(GEOID2), GEOID := GEOID2]
n_pass2 <- hq_cty[!is.na(GEOID), sum(n_hq)] - n_pass1
hq_cty[, GEOID2 := NULL]

## Two source keys can land on one county; collapse rather than fan out.
hq_by_geoid <- hq_cty[!is.na(GEOID), list(n_hq = sum(n_hq)), by = GEOID]
unmatched   <- hq_cty[is.na(GEOID)][order(-n_hq)]

## RECONCILIATION. Print this every run. A map that silently drops a hundred
## headquarters still looks completely convincing, which is the danger.
N_MAPPED <- sum(hq_by_geoid$n_hq)
cat("\n---------------- reconciliation ----------------\n")
cat(sprintf("  headquarters in file          %6s\n", comma(N_HQ_ALL)))
cat(sprintf("  less island territories       %6s\n", comma(-n_terr)))
cat(sprintf("  less non-US                   %6s\n", comma(-n_intl)))
cat(sprintf("  less missing county name      %6s\n", comma(-n_nocounty)))
cat(sprintf("  = should map                  %6s\n", comma(N_HQ_MAPPABLE)))
cat(sprintf("  actually mapped               %6s  (%.2f%%)\n",
            comma(N_MAPPED), 100 * N_MAPPED / N_HQ_MAPPABLE))
cat(sprintf("    pass 1, exact               %6s\n", comma(n_pass1)))
cat(sprintf("    pass 2, bare city name      %6s\n", comma(n_pass2)))
cat(sprintf("  SHORTFALL                     %6s\n", comma(N_HQ_MAPPABLE - N_MAPPED)))
cat("------------------------------------------------\n")
if (nrow(unmatched)) {
  cat("\nUNMATCHED -- resolve before presenting:\n")
  print(unmatched[, list(k1, n_hq, cu_names)])
} else {
  cat("no unmatched counties\n")
}

## Counties with no HQ stay in the map as empty land. On this map the empty
## half of the country is the finding -- draw it, do not omit it.
cty_map <- merge(cty_sf, as.data.frame(hq_by_geoid), by = "GEOID", all.x = TRUE)
cty_map$n_hq[is.na(cty_map$n_hq)] <- 0L
stopifnot(sum(cty_map$n_hq) == N_MAPPED)     # nothing lost or duplicated in the join

## Binned fill. A continuous scale is unreadable here -- a handful of urban
## counties compress everything else into the first colour.
cty_map$hq_bin <- cut(cty_map$n_hq,
  breaks = c(-1, 0, 1, 2, 4, 9, 19, Inf),
  labels = c("None", "1", "2", "3-4", "5-9", "10-19", "20+"))

## Centroids as an SF LAYER, not raw x/y. Mixing geom_point with geom_sf is
## what broke the earlier bubble maps: a non-sf layer is not re-projected with
## the rest. Sorted so the largest bubbles draw last.
cent <- suppressWarnings(
  st_centroid(cty_map[cty_map$n_hq > 0, ], of_largest_polygon = TRUE))
cent <- cent[order(cent$n_hq), ]

TOTAL <- sum(cty_map$n_hq)
NCTY  <- sum(cty_map$n_hq > 0)
NALL  <- nrow(cty_map)
cat(sprintf("\nMapping %s headquarters across %s of %s counties\n",
            comma(TOTAL), comma(NCTY), comma(NALL)))

CAPTION <- paste0(
  "Source: NCUA Credit Union Branch Information, 2026Q1")

## ---- 13.5  map A -- county choropleth --------------------------------------

map_a <- ggplot() +
  geom_sf(data = cty_map, aes(fill = hq_bin), colour = "white", linewidth = 0.06) +
  geom_sf(data = st_sf, fill = NA, colour = adjustcolor(PAL$ink, 0.45), linewidth = 0.28) +
  scale_fill_manual(
    values = c("None" = PAL$land, "1" = PAL$seq[2], "2" = PAL$seq[3],
               "3-4" = PAL$seq[4], "5-9" = PAL$seq[5], "10-19" = PAL$seq[6],
               "20+" = PAL$seq[7]),
    name = "Headquarters in county",
    guide = guide_legend(nrow = 1, label.position = "bottom",
                         keywidth = unit(2.4, "lines"), keyheight = unit(0.5, "lines"))) +
  labs(title = "Credit Union Headquarters",
       subtitle = sprintf(
         "",
         comma(TOTAL), comma(NCTY), comma(NALL), comma(NALL - NCTY)),
       caption = CAPTION) +
  theme_map()

## ---- 13.6  map B -- bubbles ------------------------------------------------
## The choropleth answers "which counties". The bubble map answers "how
## concentrated", which is usually the question actually being asked.
## No coord_sf() here on purpose -- see the header note. Every layer is sf and
## inherits the CRS from the data, so nothing can drift out of register.

map_b <- ggplot() +
  geom_sf(data = cty_map, fill = PAL$land, colour = "white", linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = adjustcolor(PAL$ink, 0.35), linewidth = 0.3) +
  geom_sf(data = cent, aes(size = n_hq, fill = n_hq),
          shape = 21, colour = "white", stroke = 0.35, alpha = 0.85) +
  scale_size_area(max_size = 13, breaks = c(1, 5, 10, 25, 50),
                  name = "Headquarters", guide = guide_legend(nrow = 1)) +
  scale_fill_gradientn(colours = PAL$seq[3:7], trans = "sqrt", guide = "none") +
  labs(title = "Concentration of Credit Union Headquarters",
       subtitle = sprintf(
         "One bubble per county, sized by the number of charters headquartered there. %s charters, %s counties.",
         comma(TOTAL), comma(NCTY)),
       caption = CAPTION) +
  theme_map()

## ---- 13.7  map C -- dark variant for slides --------------------------------

map_c <- ggplot() +
  geom_sf(data = cty_map, fill = "#1B2C3D", colour = "#12202E", linewidth = 0.06) +
  geom_sf(data = st_sf, fill = NA, colour = "#2E4A63", linewidth = 0.32) +
  geom_sf(data = cent, aes(size = n_hq, fill = n_hq),
          shape = 21, colour = "#0C1720", stroke = 0.3, alpha = 0.9) +
  scale_size_area(max_size = 13, breaks = c(1, 5, 10, 25, 50),
                  name = "Headquarters", guide = guide_legend(nrow = 1)) +
  scale_fill_gradientn(colours = c("#4E9AA8", "#7FD0C6", "#E8A24A", "#C8553D"),
                       trans = "sqrt", guide = "none") +
  labs(title = "Credit union headquarters, 2026Q1",
       subtitle = sprintf("%s charters across %s counties", comma(TOTAL), comma(NCTY)),
       caption = "Source: NCUA Credit Union Branch Information, 2026Q1.") +
  theme_map(dark = TRUE)

## ---- 13.8  write -----------------------------------------------------------
## 300 dpi at 14 x 9.2 in survives projection onto a wall and drops into a deck
## without resampling.

ggsave(file.path(MAP_DIR, "hq_counties_choropleth.png"), map_a,
       width = 14, height = 9.2, dpi = 300, bg = PAL$paper)
ggsave(file.path(MAP_DIR, "hq_counties_bubbles.png"), map_b,
       width = 14, height = 9.2, dpi = 300, bg = PAL$paper)
ggsave(file.path(MAP_DIR, "hq_counties_dark.png"), map_c,
       width = 14, height = 9.2, dpi = 300, bg = PAL$ink)
cat(sprintf("\nWrote three maps to %s\n", MAP_DIR))

## Backing tables -- someone always asks which county the big bubble is.
cent_dt <- as.data.table(st_drop_geometry(cent))
top_cty <- merge(cent_dt, st_dt, by.x = "STATEFP", by.y = "statefp")[
  order(-n_hq)][1:25, list(county = NAME, state = state_name, n_hq)]
cat("\n--- Top 25 counties by headquarters ---\n"); print(top_cty)
fwrite(top_cty, file.path(MAP_DIR, "hq_top_counties.csv"))

state_tot <- merge(cent_dt[, list(n_hq = sum(n_hq)), by = STATEFP],
                   st_dt, by.x = "STATEFP", by.y = "statefp")[order(-n_hq)]
fwrite(state_tot[, list(state = state_name, stusps, n_hq)],
       file.path(MAP_DIR, "hq_by_state.csv"))

if (nrow(unmatched)) fwrite(unmatched, file.path(MAP_DIR, "hq_unmatched.csv"))
