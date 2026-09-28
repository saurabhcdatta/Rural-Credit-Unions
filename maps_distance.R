###############################################################################
# maps_distance.R  --  Maps 11 and 12: distance to the nearest credit union office
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# METHOD (route B: ZIP centroids)
#   Office location  = centroid of the office's ZIP code tabulation area (Census
#                      gazetteer). Fallback: the office's county population
#                      centroid where the ZIP has no ZCTA (PO-box ZIPs).
#   Where people are = population-weighted centroid of every census tract
#                      (Census CenPop2020), each carrying its 2020 population.
#   Distance         = great-circle miles from each tract centroid to the
#                      nearest office. Straight-line, as the Federal Reserve's
#                      banking-desert work uses; road distance is longer.
#   County measures  = population-weighted mean distance, and the share of
#                      residents more than 10 miles from any office (the Fed's
#                      rural desert threshold).
#
# Upgrade path: geocode office street addresses (Census Geocoder batch) and
# replace the ZIP centroid block; nothing downstream changes.
#
# REQUIRES: rq_load.R (or the legacy shim) so sites, cr, county, QZ, CACHE_DIR,
#           dl(), cty_sf, st_sf, TH, leg(), CAP(), sv(), fac(), palette constants
#           exist -- i.e. run maps.R through its "base geometry" block first.
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2) })
stopifnot(exists("sites"), exists("county"), exists("cty_sf"), exists("QZ"))
sf_use_s2(TRUE)
## Binary-safe check for a proxy block page masquerading as a download.
## Defined here too, so this script works whichever copy of dl() is loaded.
looks_like_html <- function(f) {
  b <- readBin(f, "raw", 400L)
  if (length(b) >= 2L && b[1] == as.raw(0x50) && b[2] == as.raw(0x4b)) return(FALSE)   # ZIP
  b[b == as.raw(0)] <- as.raw(32)
  txt <- tolower(iconv(rawToChar(b), from = "", to = "ASCII", sub = ""))
  grepl("<html|<!doctype", txt)
}
MI <- 1609.344   # metres per mile

## ---- D1  ZIP centroids (gazetteer) ------------------------------------------
gz_zip <- dl("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_zcta_national.zip",
             file.path(CACHE_DIR, "2023_Gaz_zcta_national.zip"))
gz_txt <- file.path(CACHE_DIR, "2023_Gaz_zcta_national.txt")
if (!file.exists(gz_txt)) unzip(gz_zip, exdir = CACHE_DIR)
gz <- fread(gz_txt, colClasses = "character", showProgress = FALSE)
setnames(gz, trimws(tolower(names(gz))))
zcta <- gz[, .(zip5 = geoid, lon = as.numeric(intptlong), lat = as.numeric(intptlat))]
cat("ZCTA centroids:", nrow(zcta), "\n")                                           ## LOOK -- ~33,000

## county population centroids, the fallback
co_file <- dl("https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt",
              file.path(CACHE_DIR, "CenPop2020_Mean_CO.txt"))
cenco <- fread(co_file, colClasses = "character", showProgress = FALSE)
cenco <- cenco[, .(fips = paste0(STATEFP, COUNTYFP), lon = as.numeric(LONGITUDE), lat = as.numeric(LATITUDE))]

## ---- D2  offices at the latest quarter, located --------------------------------
if (!"in_cr" %in% names(sites)) sites[, in_cr := cu_number %in% unique(cr$cu_number)]
off <- sites[qidx == QZ & in_cr == TRUE & !foreign & !terr & !is.na(fips),
             .(cu_number, site_id, fips, zip5, main_office)]
off[zcta, on = "zip5", `:=`(lon = i.lon, lat = i.lat, src = "ZIP centroid")]
off[is.na(lon)][cenco, on = "fips", `:=`(lon = i.lon, lat = i.lat)]
off[is.na(src) & !is.na(lon), src := "county centroid (ZIP unmatched)"]
off <- off[!is.na(lon)]
cat("\nOffices located:", nrow(off), "\n"); print(off[, .N, by = src])              ## LOOK -- expect >95% ZIP centroid
off_sf <- st_as_sf(off, coords = c("lon", "lat"), crs = 4326)

## ---- D3  tract population centroids -------------------------------------------
tr_file <- dl("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt",
              file.path(CACHE_DIR, "CenPop2020_Mean_TR.txt"))
tr <- fread(tr_file, colClasses = "character", showProgress = FALSE)
tr <- tr[, .(tract = paste0(STATEFP, COUNTYFP, TRACTCE), fips = paste0(STATEFP, COUNTYFP),
             pop = as.numeric(POPULATION), lon = as.numeric(LONGITUDE), lat = as.numeric(LATITUDE))]
tr <- tr[pop > 0 & as.integer(substr(fips, 1, 2)) <= 56L]
tr[fips %in% c("02063", "02066"), fips := "02261"]         # Alaska fold, as everywhere else
cat("Populated tracts:", nrow(tr), "\n")                                            ## LOOK -- ~83,000
tr_sf <- st_as_sf(tr, coords = c("lon", "lat"), crs = 4326)

## ---- D4  nearest office and distance -------------------------------------------
## st_nearest_feature uses an s2 index on the sphere; ~83k x ~22k is fine.
idx <- st_nearest_feature(tr_sf, off_sf)
tr[, nearest_office := idx]
tr[, miles := as.numeric(st_distance(tr_sf, off_sf[idx, ], by_element = TRUE)) / MI]
cat("\nTract-level distance, miles:\n"); print(summary(tr$miles))                    ## LOOK

## ---- D5  county measures ---------------------------------------------------------
cd <- tr[, .(pop = sum(pop),
             miles_wmean = sum(miles * pop) / sum(pop),
             miles_median_wtd = { o <- order(miles); cw <- cumsum(pop[o]) / sum(pop); miles[o][which(cw >= 0.5)[1]] },
             share_over_10 = sum(pop[miles > 10]) / sum(pop),
             share_over_20 = sum(pop[miles > 20]) / sum(pop)), by = fips]
cd <- merge(cd, county[, .(fips, rural24, ct, is_state_or_dc, county_name, state)], by = "fips", all.x = TRUE)
cd <- cd[is_state_or_dc == TRUE & !ct]

cat("\n=== Distance to nearest credit union office, by segment ===\n")
seg <- cd[, .(counties = .N,
              median_county_wmean_miles = round(median(miles_wmean), 1),
              residents_over_10mi_m = round(sum(pop * share_over_10) / 1e6, 2),
              pct_residents_over_10mi = round(100 * sum(pop * share_over_10) / sum(pop), 1),
              pct_residents_over_20mi = round(100 * sum(pop * share_over_20) / sum(pop), 1)),
          by = .(segment = fifelse(rural24 == 1L, "Rural", "Non-rural"))]
print(seg)                                                                            ## LOOK -- the report numbers
if (exists("register_result")) for (i in seq_len(nrow(seg))) {
  register_result(8, paste0("pct_residents_over_10mi_", tolower(seg$segment[i])), seg$pct_residents_over_10mi[i], "ZIP-centroid method, tract population centroids")
  register_result(8, paste0("residents_over_10mi_m_", tolower(seg$segment[i])), seg$residents_over_10mi_m[i], "millions")
}

## ---- MAP 11  average distance, rural counties -----------------------------------
K2 <- merge(county[is_state_or_dc == TRUE, .(fips, rural24)], cd[, .(fips, miles_wmean, share_over_10, pop)], by = "fips", all.x = TRUE)
K2[, m11 := fifelse(rural24 == 0L, "Non-rural",
           as.character(cut(miles_wmean, c(-Inf, 5, 10, 20, 40, Inf),
                            labels = c("Under 5 miles", "5-10 miles", "10-20 miles", "20-40 miles", "Over 40 miles"))))]
K2[, m11 := fac(m11, c("Non-rural", "Under 5 miles", "5-10 miles", "10-20 miles", "20-40 miles", "Over 40 miles"))]
M11 <- merge(cty_sf, K2, by = "fips", all.x = TRUE)
far_n <- K2[rural24 == 1L & miles_wmean > 20, .N]; far_pop <- K2[rural24 == 1L & miles_wmean > 20, sum(pop, na.rm = TRUE)]
p11 <- ggplot() +
  geom_sf(data = M11, aes(fill = m11), colour = C_CTY, linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "Under 5 miles" = "#DCE6E4", "5-10 miles" = "#9FC3C1",
                               "10-20 miles" = "#E8A24A", "20-40 miles" = "#C8553D", "Over 40 miles" = "#7A2A1A"),
                    name = "Average distance from a resident to the nearest credit union office", drop = FALSE) +
  labs(title = sprintf("In %d rural counties the average resident is more than 20 miles from a credit union office \u2014 %.1f million people", far_n, far_pop / 1e6),
       subtitle = "Population-weighted average, from census-tract population centres to the nearest office, straight-line miles. Rural counties only.",
       caption = CAP("Office locations: ZIP-code centroids (Census gazetteer 2023). Population: Census 2020 tract centroids.")) + TH + leg(1)
sv(p11, "map11_distance_to_nearest_office")

## ---- MAP 12  the Fed banking-desert standard: share of residents over 10 miles ----
K2[, m12 := fifelse(rural24 == 0L, "Non-rural",
           as.character(cut(100 * share_over_10, c(-Inf, 10, 25, 50, 75, 100.001),
                            labels = c("Under 10%", "10-25%", "25-50%", "50-75%", "Over 75%"))))]
K2[, m12 := fac(m12, c("Non-rural", "Under 10%", "10-25%", "25-50%", "50-75%", "Over 75%"))]
M12 <- merge(cty_sf, K2, by = "fips", all.x = TRUE)
des_n <- K2[rural24 == 1L & share_over_10 > 0.5, .N]
des_pop <- cd[rural24 == 1L, sum(pop * share_over_10)]
p12 <- ggplot() +
  geom_sf(data = M12, aes(fill = m12), colour = C_CTY, linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "Under 10%" = "#DCE6E4", "10-25%" = "#9FC3C1",
                               "25-50%" = "#E8A24A", "50-75%" = "#C8553D", "Over 75%" = "#7A2A1A"),
                    name = "Share of residents more than 10 miles from any credit union office", drop = FALSE) +
  labs(title = sprintf("%.1f million rural residents live more than 10 miles from a credit union office \u2014 the Federal Reserve's rural \u201Cdesert\u201D threshold", des_pop / 1e6),
       subtitle = sprintf("In %d rural counties, more than half of residents are beyond 10 miles. Same 10-mile rural standard as the interagency banking-desert work, so comparable with the bank study.", des_n),
       caption = CAP("Office locations: ZIP-code centroids (Census gazetteer 2023). Population: Census 2020 tract centroids. Threshold: 10 miles straight-line for rural tracts, per FedCommunities Banking Deserts methodology.")) + TH + leg(1)
sv(p12, "map12_residents_beyond_10_miles")

fwrite(cd, file.path(MAP_DIR, "county_distance_to_nearest_office.csv"))
cat("\nMaps 11 and 12 written. County table saved for Q8.\n")
