###############################################################################
# maps_rq6.R  --  Map 17: Who lends to farms
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# WHAT IT SAYS
#   One dot per credit union that lends to farms (AgLending definition: any farm
#   balance or year-to-date farm lending in the past two years), at its
#   headquarters county, sized by farm loans and coloured rural vs non-rural.
#   Underneath: rural counties, with USDA farming-dependent counties picked out.
#   The picture: rural credit unions lending to farms sit in farm country; most
#   credit-union farm lenders do not.
#
# REQUIRES  maps.R run through MAP 9 in this session (cty_sf, st_sf, cent_dt, K
#           with `farming`, TH, CAP, sv, palette), and rq6.R run (it writes
#           rq6_farm_lenders_latest.csv).
#
# NOTE  Dots sit at the credit union's HEADQUARTERS county. The call report does
#       not say where the farms are, so the caption says "credit unions that
#       lend to farms", never "where farm loans are made".
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(sf) })
stopifnot("run maps.R through MAP 9 first" = exists("cty_sf") && exists("cent_dt") && exists("K") && "farming" %in% names(K))

## ---- 17.1  the farm lenders (from rq6.R) ---------------------------------------
f17 <- file.path(OUT_DIR, "rq6", "rq6_farm_lenders_latest.csv")
stopifnot("run rq6.R first -- it writes rq6_farm_lenders_latest.csv" = file.exists(f17))
fl <- fread(f17, colClasses = c(fips = "character"))
fl[, fips := sprintf("%05s", fips)][, fips := gsub(" ", "0", fips)]
all_cu <- copy(fl)                                                               # every CU, lender or not
fl <- fl[lends_to_farms == TRUE]
fl[, group := factor(ifelse(rural == 1L, "Rural credit union", "Non-rural credit union"),
                     levels = c("Rural credit union", "Non-rural credit union"))]
fl[, farm_m := pmax(ag_total, 0) / 1e6]
pts <- merge(fl, cent_dt, by = "fips")                                           # territories have no shape and drop here
cat("Farm lenders:", nrow(fl), "| on the map:", nrow(pts), "\n")                 ## LOOK -- 476 / about 476
pts[, .(lenders = .N, farm_loans_bn = round(sum(farm_m) / 1e3, 2)), by = group] |> print()   ## LOOK

## ---- 17.2  the base: rural counties, farming-dependent ones darker ----------------
K17 <- K[, .(fips, base = fifelse(rural24 == 0L, "Non-rural county",
                          fifelse(farming %in% TRUE, "Rural, farming-dependent", "Rural, other")))]
BASE_COLS <- c("Non-rural county" = C_NONRURAL, "Rural, other" = "#E6DDC6", "Rural, farming-dependent" = "#B08D57")
K17[, base := factor(base, levels = names(BASE_COLS))]
M17 <- merge(cty_sf, K17, by = "fips", all.x = TRUE)

## headline numbers for the title, computed, never typed
n_r  <- pts[group == "Rural credit union", .N];  n_o <- pts[group == "Non-rural credit union", .N]
sh_r <- round(100 * pts[group == "Rural credit union", sum(farm_m)] / pts[, sum(farm_m)])
ff   <- merge(all_cu[rural == 1L, .(cu_number, fips, lends_to_farms)], K[, .(fips, farming)], by = "fips", all.x = TRUE)   # all rural CUs

## ---- 17.3  the map ----------------------------------------------------------------
DOT_COLS <- c("Rural credit union" = "#0E4C55", "Non-rural credit union" = "#E69F00")   # teal vs Okabe-Ito orange
p17 <- ggplot() +
  geom_sf(data = M17, aes(fill = base), colour = C_CTY, linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = BASE_COLS, name = NULL, na.value = C_NONRURAL) +
  geom_point(data = pts[order(-farm_m)], aes(X, Y, size = farm_m, colour = group), alpha = 0.8) +
  scale_colour_manual(values = DOT_COLS, name = NULL) +
  scale_size_area(max_size = 9, breaks = c(1, 10, 50, 150), labels = c("$1m", "$10m", "$50m", "$150m"), name = "Farm loans") +
  labs(title = sprintf("%d credit unions lend to farms: %d rural, %d non-rural\nRural ones hold %d%% of credit-union farm loans", n_r + n_o, n_r, n_o, sh_r),
       subtitle = sprintf(paste0("One dot per credit union that lends to farms, at its headquarters county, sized by farm loans. ",
                                 "Darker counties: USDA farming-dependent.\nOf the %d rural credit unions in farming-dependent counties, %d lend to farms."),
                          ff[farming %in% TRUE, uniqueN(cu_number)],
                          ff[farming %in% TRUE & lends_to_farms == TRUE, uniqueN(cu_number)]),
       caption = CAP("Farm loans = agricultural commercial loans (5300 accounts 042A5-042A9), incl. participations bought from other lenders; lender = any balance or YTD farm lending in the past two years (AgLending definition). Headquarters county, not farm location.")) +
  TH + guides(fill = guide_legend(order = 1, nrow = 1, title.position = "top"),
              colour = guide_legend(order = 2, nrow = 1, override.aes = list(size = 4)),
              size = guide_legend(order = 3, nrow = 1, title.position = "top"))
sv(p17, "map17_who_lends_to_farms")
cat("Map 17 written to", MAP_DIR, "\n")
