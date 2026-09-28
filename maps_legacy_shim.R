###############################################################################
# maps_legacy_shim.R  --  run BEFORE maps.R when the master build is not yet made
#
# Builds county / sites / offices_q / cu from the old pipeline objects
# (u24, u13, cr, b) plus the two Census population CSVs. This is a bridge, not
# a replacement for M0-M5: the master build adds the checkpoints and the
# frozen version. Once the master exists, use rq_load.R and delete this.
#
# Needs in the environment: u24, u13, cr (from 1_ 2_ 3_), and b (from 5_) or
# the cached _branch_panel.rds. CACHE_DIR / DATA_DIR / OUT_DIR from 1_setup.R.
###############################################################################

suppressPackageStartupMessages(library(data.table))
stopifnot(exists("u24"), exists("u13"), exists("cr"), exists("DATA_DIR"), exists("CACHE_DIR"), exists("OUT_DIR"))
if (!exists("b")) b <- readRDS(file.path(DATA_DIR, "_branch_panel.rds"))

## helpers maps.R expects
q_year <- function(q) (q - 1L) %/% 4L
q_qtr  <- function(q) ((q - 1L) %% 4L) + 1L
q_lab  <- function(q) sprintf("%dQ%d", q_year(q), q_qtr(q))
if (!exists("dl")) dl <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(invisible(dest))
  for (m in c("curl", "libcurl", "auto")) {
    ok <- tryCatch({ download.file(url, dest, mode = "wb", method = m, quiet = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 0) {
      if (any(grepl("<html|<!doctype", tolower(readLines(dest, n = 2, warn = FALSE))))) { unlink(dest); next }
      return(invisible(dest))
    }
  }
  stop("Could not download ", url, " -- save it manually as ", dest)
}
ADJ_CLASS_2024 <- c(`3` = "adjacent to large metro", `6` = "adjacent to small metro",
                    `7` = "remote micropolitan", `8` = "remote", `9` = "remote")

## ---- county -----------------------------------------------------------------
county <- copy(u24)[, .(fips, state, county_name, uic24 = uic, rural24 = rural, pop2020 = pop)]
county[u13, on = "fips", `:=`(uic13 = i.uic, rural13 = i.rural)]
county[, adj_class := fifelse(rural24 == 1L, ADJ_CLASS_2024[as.character(uic24)], NA_character_)]

## population 2024 from the Census bulk file (same one 14_ used); fall back to 2020
p24_file <- file.path(CACHE_DIR, "co-est2024-alldata.csv")
if (!file.exists(p24_file))
  try(dl("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv", p24_file), silent = TRUE)
if (file.exists(p24_file)) {
  p24 <- fread(p24_file, colClasses = "character", encoding = "Latin-1", showProgress = FALSE)
  p24 <- p24[SUMLEV == "050", .(fips = paste0(STATE, COUNTY), pop2024 = as.numeric(POPESTIMATE2024))]
  p24[fips %in% c("02063", "02066"), fips := "02261"]; p24 <- p24[, .(pop2024 = sum(pop2024)), by = fips]
  county[p24, on = "fips", pop2024 := i.pop2024]
} else county[, pop2024 := NA_real_]
county[, `:=`(is_state_or_dc = as.integer(substr(fips, 1, 2)) <= 56L, ct = state == "CT")]
setkey(county, fips)

## ---- sites and offices_q ----------------------------------------------------
sites <- b
if (!"foreign" %in% names(sites)) sites[, foreign := FALSE]
if (!"terr"    %in% names(sites)) sites[, terr := FALSE]
offices_q <- sites[!foreign & !terr & !is.na(fips) & !is.na(rural_site),
                   .(offices = .N, hqs = sum(main_office), branches = sum(!main_office), cus = uniqueN(cu_number)),
                   by = .(fips, qidx)]
setkey(offices_q, fips, qidx)

## ---- cu ---------------------------------------------------------------------
cu <- cr[, .(first_q = min(qidx), last_q = max(qidx),
             rural_first = rural[which.min(qidx)], rural_last = rural[which.max(qidx)],
             fips_first = fips[which.min(qidx)], fips_last = fips[which.max(qidx)]), by = cu_number]
setkey(cu, cu_number)

## ---- the quarter every map is drawn at ---------------------------------------
QZ <- min(max(cr$qidx), max(offices_q$qidx))     # latest quarter both tables cover
cat("Shim ready. county:", nrow(county), "| sites:", format(nrow(sites), big.mark = ","),
    "| offices_q:", nrow(offices_q), "| cu:", nrow(cu), "| maps drawn at", q_lab(QZ), "\n")
cat("Now run maps.R.\n")
