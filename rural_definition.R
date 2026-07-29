###############################################################################
# rural_definition.R
#
# Statutory rural classification for the ROAD Act Sec. 909 study.
#
# DEFINITION AS ENACTED -- 12 CFR 1026.35(b)(2)(iv)(A)(1), incorporated by
# reference into ROAD Act Sec. 909:
#
#   "A county that is neither in a metropolitan statistical area nor in a
#    micropolitan statistical area that is adjacent to a metropolitan
#    statistical area, as those terms are defined by the U.S. Office of
#    Management and Budget and as they are applied under currently applicable
#    Urban Influence Codes (UICs), established by the United States Department
#    of Agriculture's Economic Research Service (USDA-ERS)"
#
# So a county is RURAL if it is:
#     NOT metropolitan, AND
#     NOT (micropolitan AND adjacent to a metro area)
# which admits:
#     - noncore counties, regardless of adjacency
#     - micropolitan counties that are NOT adjacent to a metro area
#
# Data: USDA-ERS Urban Influence Codes
#   2024 (current): https://www.ers.usda.gov/media/6182/2024-urban-influence-codes.csv
#   2013 (prior):   https://www.ers.usda.gov/media/6183/2013-urban-influence-codes.xls
###############################################################################

library(data.table)

## ---------------------------------------------------------------------------
## 1. CODE MAPPINGS
## ---------------------------------------------------------------------------
## The 2024 UIC was consolidated from 12 categories to 9 AND renumbered.
## The code sets below are NOT interchangeable across vintages.

UIC_2024 <- data.table(
  uic = 1:9,
  label = c(
    "Large metro (metro area >= 1M residents)",
    "Micropolitan, adjacent to a large metro area",
    "Noncore, adjacent to a large metro area",
    "Small metro (metro area < 1M residents)",
    "Micropolitan, adjacent to a small metro area",
    "Noncore, adjacent to a small metro area",
    "Micropolitan, NOT adjacent to a metro area",
    "Noncore, not adjacent to metro, town >= 5,000",
    "Noncore, not adjacent to metro, no town >= 5,000"
  ),
  metro       = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  micro       = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
  metro_adj   = c(NA, TRUE, TRUE, NA, TRUE, TRUE, FALSE, FALSE, FALSE)
)
UIC_2024[, rural := as.integer(!metro & !(micro & metro_adj %in% TRUE))]
#  -> RURAL = codes 3, 6, 7, 8, 9

UIC_2013 <- data.table(
  uic = 1:12,
  label = c(
    "Large metro (>= 1M)",
    "Small metro (< 1M)",
    "Micropolitan, adjacent to a large metro area",
    "Noncore, adjacent to a large metro area",
    "Micropolitan, adjacent to a small metro area",
    "Noncore, adjacent to small metro, town >= 2,500",
    "Noncore, adjacent to small metro, no town >= 2,500",
    "Micropolitan, NOT adjacent to a metro area",
    "Noncore, adjacent to micro, town >= 2,500",
    "Noncore, adjacent to micro, no town >= 2,500",
    "Noncore, not adjacent to metro/micro, town >= 2,500",
    "Noncore, not adjacent to metro/micro, no town >= 2,500"
  ),
  metro     = c(TRUE, TRUE, rep(FALSE, 10)),
  micro     = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, rep(FALSE, 4)),
  metro_adj = c(NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
)
UIC_2013[, rural := as.integer(!metro & !(micro & metro_adj %in% TRUE))]
#  -> RURAL = codes 4, 6, 7, 8, 9, 10, 11, 12

RURAL_CODES <- list(`2024` = UIC_2024[rural == 1, uic],   # 3,6,7,8,9
                    `2013` = UIC_2013[rural == 1, uic])   # 4,6,7,8,9,10,11,12

## ---------------------------------------------------------------------------
## 2. WHY THE TWO VINTAGES ARE MORE COMPARABLE THAN ERS WARNS
## ---------------------------------------------------------------------------
## ERS cautions that the 2024 codes are not comparable to 2013. That is true of
## the 9- vs 12-category SCHEME. It is not true of the rural BINARY, because
## every 2013->2024 consolidation happened within a rural-status class:
##
##   2024 1 <- 2013 1        (not rural -> not rural)
##   2024 2 <- 2013 3        (not rural -> not rural)
##   2024 3 <- 2013 4        (rural     -> rural)
##   2024 4 <- 2013 2        (not rural -> not rural)
##   2024 5 <- 2013 5        (not rural -> not rural)
##   2024 6 <- 2013 6, 7     (rural     -> rural)
##   2024 7 <- 2013 8        (rural     -> rural)
##   2024 8 <- 2013 9, 11    (rural     -> rural)
##   2024 9 <- 2013 10, 12   (rural     -> rural)
##
## The crosswalk is rural-status preserving. The town-size threshold change
## (2,500 -> 5,000) only splits categories that are rural on both sides, so it
## cannot move a county across the statutory line.
##
## CONSEQUENCE: any county whose rural status differs between the 2013 and 2024
## UIC changed because OMB REDELINEATED it (metro/micro status or commuting-
## based metro adjacency), not because ERS restructured the codes. That makes
## the reclassification component of Q2 cleanly attributable. Use
## compare_vintages() below to produce it.

## ---------------------------------------------------------------------------
## 3. VALIDATION BENCHMARKS (published by ERS, 50 states + DC)
## ---------------------------------------------------------------------------
## Reproduce these before trusting any merge. If your county counts differ,
## the problem is the FIPS join, not the definition.

ERS_2024_BENCH <- data.table(
  uic = 1:9,
  n_counties = c(443, 130, 154, 743, 272, 490, 256, 125, 531),
  pop_2020   = c(189022706, 7492847, 3253097, 96626916, 12185521,
                 8100787, 8257219, 2336128, 4174060)
)
# Implied statutory totals, 50 states + DC:
#   RURAL counties (codes 3,6,7,8,9): 1,556 of 3,144  (49.5%)
#   RURAL population:                 26,121,291 of 331,449,281  (7.9%)
#
# Note the asymmetry -- roughly half of all counties, but under 8% of people.
# That is the single most important context number for the whole report, and
# it belongs in the executive summary.

## ---------------------------------------------------------------------------
## 4. FETCH (download once, pin, then always read the local copy)
## ---------------------------------------------------------------------------
## There is no ERS API for the UICs -- ERS's REST APIs cover ARMS and the GIS
## map services only. These are static files, revised roughly once per decade.
##
## For a report to Congress, auto-downloading on every run is a LIABILITY, not
## a convenience: if ERS silently re-releases a file, your numbers change and
## nothing tells you. The right pattern is fetch once, record the SHA-256 in a
## manifest, and read the cached copy thereafter.

UIC_URLS <- list(
  `2024` = "https://www.ers.usda.gov/media/6182/2024-urban-influence-codes.csv",
  `2013` = "https://www.ers.usda.gov/media/6183/2013-urban-influence-codes.xls"
)

## Is the file on disk actually the data, or a proxy block page / login form?
## A managed network often returns HTML with HTTP 200, which download.file
## reports as success. This is the single most common silent failure.
.uic_file_ok <- function(path, min_bytes = 10000) {
  if (!file.exists(path))          return(list(ok = FALSE, why = "file does not exist"))
  sz <- file.size(path)
  if (sz < min_bytes)              return(list(ok = FALSE,
                                     why = sprintf("only %s bytes -- too small to be the UIC file", format(sz, big.mark = ","))))
  con <- file(path, "rb"); raw512 <- readBin(con, "raw", 512); close(con)
  ## byte-level checks only -- never coerce unknown bytes through tolower(),
  ## which throws "invalid multibyte string" on Windows
  head_txt <- rawToChar(raw512[raw512 != as.raw(0)])
  if (grepl("<html|<!doctype|<head>|sign in|access denied|blocked", head_txt,
            ignore.case = TRUE, useBytes = TRUE))
    return(list(ok = FALSE, why = "content is HTML -- almost certainly a proxy block page or login redirect, not data"))
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    l1 <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
    if (!grepl("fips", l1, ignore.case = TRUE, useBytes = TRUE))
      return(list(ok = FALSE, why = paste0("CSV header has no FIPS column; first line was: ", substr(l1, 1, 80))))
  }
  list(ok = TRUE, why = "")
}

.rule <- function(ch = "-") cat(strrep(ch, 74), "\n", sep = "")

fetch_uic <- function(vintage = c("2024", "2013"),
                      cache_dir = "data/raw",
                      manifest  = "data/data_manifest.csv",
                      force     = FALSE) {
  vintage <- match.arg(vintage)
  url  <- UIC_URLS[[vintage]]
  dest <- file.path(cache_dir, basename(url))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  abs_dest <- normalizePath(dest, winslash = "/", mustWork = FALSE)

  .rule("="); cat("  ERS Urban Influence Codes -- ", vintage, " vintage\n", sep = ""); .rule("=")
  cat("  working directory : ", getwd(), "\n", sep = "")
  cat("  target file       : ", abs_dest, "\n", sep = "")
  cat("  source URL        : ", url, "\n", sep = "")
  .rule()

  ## --- already cached? -------------------------------------------------
  if (file.exists(dest) && !force) {
    chk <- .uic_file_ok(dest)
    if (chk$ok) {
      cat("  STATUS: OK -- using the cached copy. No download needed.\n")
      cat("          size ", format(file.size(dest), big.mark = ","), " bytes, ",
          "retrieved ", format(file.mtime(dest), "%Y-%m-%d"), "\n", sep = "")
      .rule("="); cat("\n")
      return(.uic_pin(dest, url, vintage, manifest))
    }
    cat("  Cached file is INVALID (", chk$why, ").\n", sep = "")
    cat("  Deleting it and retrying the download.\n"); .rule()
    unlink(dest)
  }

  ## --- attempt download ------------------------------------------------
  methods <- c("libcurl", if (.Platform$OS.type == "windows") "wininet", "curl", "wget")
  attempts <- list()

  for (m in methods) {
    cat("  trying method = '", m, "' ... ", sep = ""); utils::flush.console()
    err <- NULL
    tryCatch(
      utils::download.file(url, dest, mode = "wb", method = m, quiet = TRUE),
      error   = function(e) err <<- conditionMessage(e),
      warning = function(w) err <<- conditionMessage(w)
    )
    ## A warning does not always mean failure, so judge by the file itself.
    chk <- .uic_file_ok(dest)
    if (!chk$ok) {
      why <- if (!is.null(err)) err else chk$why
      cat("FAILED\n      reason: ", why, "\n", sep = "")
      attempts[[m]] <- why
      unlink(dest); next
    }
    cat("SUCCESS\n"); .rule()
    cat("  STATUS: DOWNLOADED OK. Nothing further for you to do.\n")
    cat("          saved  ", abs_dest, "\n", sep = "")
    cat("          size   ", format(file.size(dest), big.mark = ","), " bytes\n", sep = "")
    .rule("="); cat("\n")
    return(.uic_pin(dest, url, vintage, manifest))
  }

  ## --- all methods failed ----------------------------------------------
  .rule("=")
  cat("  STATUS: AUTOMATIC DOWNLOAD FAILED -- MANUAL DOWNLOAD REQUIRED\n")
  .rule("=")
  cat("\n  Every method failed. On a managed federal workstation this is\n")
  cat("  normally the proxy or TLS inspection, not a problem with the code.\n\n")
  cat("  WHAT TO DO -- 3 steps, takes about a minute:\n\n")
  cat("   1. Open this link in your browser:\n        ", url, "\n\n", sep = "")
  cat("   2. Save the file, without renaming it, to exactly:\n        ",
      abs_dest, "\n      (the folder already exists -- this code just created it)\n\n", sep = "")
  cat("   3. Re-run the same call. It will find the file and continue:\n")
  cat("        uic <- load_uic(fetch_uic(\"", vintage, "\"), \"", vintage, "\")\n\n", sep = "")
  cat("  The filename must stay exactly:  ", basename(url), "\n\n", sep = "")
  cat("  OPTIONAL -- to make automatic download work next time, try this\n")
  cat("  first and then re-run (it uses the Windows certificate store, which\n")
  cat("  has your agency's root CA; libcurl does not):\n")
  cat("        options(download.file.method = \"wininet\")\n\n")
  cat("  If the link returns 404, ERS re-released the file under a new media\n")
  cat("  ID. Get the current link from:\n")
  cat("        https://www.ers.usda.gov/data-products/urban-influence-codes\n")
  cat("  and update UIC_URLS at the top of rural_definition.R.\n\n")
  cat("  Attempt log:\n")
  for (m in names(attempts)) cat("    ", m, ": ", attempts[[m]], "\n", sep = "")
  .rule("="); cat("\n")

  stop("UIC ", vintage, " not available. Follow the manual download steps printed above.",
       call. = FALSE)
}

## Record the file in the data manifest and warn if it changed since pinning.
.uic_pin <- function(dest, url, vintage, manifest) {
  sha <- if (requireNamespace("digest", quietly = TRUE))
    digest::digest(dest, algo = "sha256", file = TRUE) else NA_character_

  rec <- data.table(dataset = paste0("ers_uic_", vintage), url = url,
                    local_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
                    sha256 = sha, bytes = file.size(dest),
                    retrieved = format(file.mtime(dest), "%Y-%m-%d"))

  if (file.exists(manifest)) {
    ## read as character: fread types "2026-07-28" as IDate (integer-backed),
    ## and rbind against a character column then coerces to NA with a warning
    man <- fread(manifest, colClasses = "character")
    rec[, (names(rec)) := lapply(.SD, as.character)]
    prior <- man[dataset == rec$dataset]
    if (nrow(prior) && !is.na(sha) && !is.na(prior$sha256[1]) && prior$sha256[1] != sha)
      warning("!! ", rec$dataset, " CHANGED since it was pinned.\n",
              "   was: ", prior$sha256[1], "\n   now: ", sha, "\n",
              "   Re-run the classification and check compare_vintages() ",
              "before trusting any prior results.", call. = FALSE)
    man <- rbind(man[dataset != rec$dataset], rec, fill = TRUE)
  } else {
    dir.create(dirname(manifest), recursive = TRUE, showWarnings = FALSE)
    man <- rec
  }
  fwrite(man, manifest)
  invisible(dest)
}

## What is in the cache right now? Run this any time to check status.
uic_status <- function(cache_dir = "data/raw") {
  .rule("="); cat("  UIC cache status\n"); .rule("=")
  cat("  working directory: ", getwd(), "\n", sep = "")
  cat("  cache directory  : ", normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
      if (dir.exists(cache_dir)) "\n" else "   (DOES NOT EXIST YET)\n", sep = "")
  .rule()
  for (v in names(UIC_URLS)) {
    f <- file.path(cache_dir, basename(UIC_URLS[[v]]))
    chk <- .uic_file_ok(f)
    cat("  ", v, ": ", if (chk$ok) "OK -- ready to use" else paste0("NOT USABLE -- ", chk$why), "\n", sep = "")
    cat("        expected at ", normalizePath(f, winslash = "/", mustWork = FALSE), "\n", sep = "")
  }
  .rule("="); cat("\n")
  invisible(NULL)
}

## ---------------------------------------------------------------------------
## 5. LOADER
## ---------------------------------------------------------------------------
## ERS ships these two vintages in DIFFERENT SHAPES:
##
##   2024 CSV  -- LONG. One row per county x attribute:
##       FIPS-UIC | State | County_Name | Attribute      | Value
##       1001     | AL    | Autauga ... | Population_2020| 58805
##       1001     | AL    | Autauga ... | UIC_2024       | 4
##       1001     | AL    | Autauga ... | Description    | Small metro ...
##
##   2013 XLS  -- WIDE. One row per county, with a UIC_2013 column.
##
## The loader detects the shape and reshapes long -> wide when needed.

.norm_names <- function(x) {
  n <- tolower(trimws(names(x)))
  n <- gsub("[^a-z0-9]+", "_", n)      # "FIPS-UIC" -> "fips_uic"
  n <- gsub("^_+|_+$", "", n)
  setnames(x, n)
  x[]
}

.read_any <- function(path) {
  if (grepl("\\.(csv|txt)$", path, ignore.case = TRUE)) {
    x <- fread(path, colClasses = "character", showProgress = FALSE)
  } else {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Reading .xls requires readxl. Run: install.packages(\"readxl\")", call. = FALSE)
    x <- setDT(readxl::read_excel(path, col_types = "text"))
  }
  .norm_names(x)
}

.pick <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(p, nms, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

## Inspect a downloaded file without parsing it -- use this when a load fails.
peek_uic_file <- function(path) {
  x <- .read_any(path)
  cat("\ncolumns: ", paste(names(x), collapse = ", "), "\n", sep = "")
  cat("rows   : ", nrow(x), "\n", sep = "")
  if ("attribute" %in% names(x))
    cat("attribute values: ", paste(unique(x$attribute), collapse = ", "), "\n", sep = "")
  cat("\nfirst rows:\n"); print(head(x, 6))
  invisible(x)
}

load_uic <- function(path, vintage = c("2024", "2013")) {
  vintage <- match.arg(vintage)
  x <- .read_any(path)

  ## --- reshape long -> wide if the file is stacked ------------------------
  if (all(c("attribute", "value") %in% names(x))) {
    id <- setdiff(names(x), c("attribute", "value"))
    message("Long format detected; reshaping on: ", paste(id, collapse = ", "))
    x <- dcast(x, stats::as.formula(paste(paste(id, collapse = " + "), "~ attribute")),
               value.var = "value")
    x <- .norm_names(x)      # UIC_2024 -> uic_2024
  }

  ## --- resolve columns ----------------------------------------------------
  fips_col <- .pick(names(x), c("^fips$", "^fipstxt$", "^fips_code$", "^fips_uic$", "^fips"))
  uic_col  <- .pick(names(x), c(paste0("^uic_", vintage, "$"), "^uic$",
                                "^uic_[0-9]{4}$", "^uic", "urban_influence"))

  if (is.na(fips_col) || is.na(uic_col))
    stop("Could not resolve FIPS/UIC columns for the ", vintage, " vintage.\n",
         "  columns found: ", paste(names(x), collapse = ", "), "\n",
         "  Run peek_uic_file(\"", path, "\") to inspect the file, then pass\n",
         "  the right names explicitly if ERS has changed the layout.",
         call. = FALSE)

  message("Using columns -- FIPS: '", fips_col, "'  UIC: '", uic_col, "'")

  ## coerce first, then filter -- sprintf() on NA yields the string "NA",
  ## which would silently survive an is.na() check
  pop_col <- .pick(names(x), c("^population_[0-9]{4}$", "^pop_[0-9]{4}$", "^population$"))
  if (!is.na(pop_col)) message("Carrying population column: '", pop_col, "'")

  out <- x[, .(fips_int = suppressWarnings(as.integer(get(fips_col))),
               uic      = suppressWarnings(as.integer(get(uic_col))),
               pop      = if (!is.na(pop_col)) suppressWarnings(as.numeric(get(pop_col))) else NA_real_)]
  dropped <- out[is.na(fips_int) | is.na(uic), .N]
  if (dropped) message("Dropped ", dropped, " rows with unparseable FIPS or UIC")
  out <- out[!is.na(fips_int) & !is.na(uic)]
  out[, fips := sprintf("%05d", fips_int)][, fips_int := NULL]
  setcolorder(out, c("fips", "uic", "pop"))
  out <- unique(out, by = "fips")

  valid <- if (vintage == "2024") 1:9 else 1:12
  bad <- setdiff(unique(out$uic), valid)
  if (length(bad))
    stop("UIC values outside the ", vintage, " range (", paste(bad, collapse = ", "),
         ") -- wrong vintage file, or wrong column picked.", call. = FALSE)

  out[, rural := as.integer(uic %in% RURAL_CODES[[vintage]])]
  out[, vintage := vintage]

  message(sprintf("Loaded %s UIC: %d counties, %d rural (%.1f%%)",
                  vintage, nrow(out), sum(out$rural), 100 * mean(out$rural)))
  out[]
}

validate_uic <- function(uic_dt) {
  obs <- uic_dt[as.integer(substr(fips, 1, 2)) <= 56, .N, by = uic][order(uic)]
  cmp <- merge(ERS_2024_BENCH[, .(uic, expected = n_counties)], obs, by = "uic", all = TRUE)
  setnames(cmp, "N", "observed")
  cmp[, diff := observed - expected]
  print(cmp)
  n_rural <- uic_dt[as.integer(substr(fips, 1, 2)) <= 56 & rural == 1, .N]
  cat("\n  rural counties (50 states + DC): ", n_rural, "   [ERS-implied: 1556]\n", sep = "")
  if (isTRUE(any(cmp$diff != 0, na.rm = TRUE)))
    warning("County counts differ from ERS published figures. Check the FIPS ",
            "filter (territories, Connecticut planning regions).", call. = FALSE)
  invisible(cmp)
}

## ---------------------------------------------------------------------------
## 6. ATTACH TO THE CALL REPORT PANEL
## ---------------------------------------------------------------------------

build_fips <- function(dt, state_col = "state_code", county_col = "county_code") {
  dt[, fips := sprintf("%02d%03d", as.integer(get(state_col)), as.integer(get(county_col)))]
  dt[]
}

attach_rural <- function(dt, uic_dt, label = "rural") {
  u <- uic_dt[, .(fips, uic_code = uic, r = rural)]   # population stays on uic_dt
  out <- merge(dt, u, by = "fips", all.x = TRUE, sort = FALSE)
  setnames(out, c("r", "uic_code"), c(label, paste0("uic_", label)))

  mr <- 100 * mean(!is.na(out[[label]]))
  message(sprintf("FIPS match rate: %.2f%%  (unmatched CU-quarters: %d)",
                  mr, sum(is.na(out[[label]]))))
  if (mr < 99) {
    bad <- out[is.na(get(label)), .N, by = .(fips, state_code)][order(-N)][1:15]
    message("Top unmatched FIPS:"); print(bad)
  }
  out[]
}

## ---------------------------------------------------------------------------
## FIPS HARMONIZATION -- counties that were renamed, split, or absorbed
## ---------------------------------------------------------------------------
## A credit union headquartered in one of these will silently DROP from the
## analysis if the historical FIPS is joined against the 2024 UIC. Silent
## dropping is worse than misclassification: nothing warns you.

FIPS_CHANGES <- data.table(
  old_fips = c("02261","02261","02270","46113","51515","12025","30113","51780"),
  new_fips = c("02063","02066","02158","46102","51019","12086","30067","51083"),
  kind     = c("split","split","rename","rename","absorbed","rename","resolved","absorbed"),
  note = c(
    "Valdez-Cordova Census Area AK dissolved 2019 -> Chugach Census Area",
    "Valdez-Cordova Census Area AK dissolved 2019 -> Copper River Census Area",
    "Wade Hampton Census Area AK renamed 2015 -> Kusilvak Census Area",
    "Shannon County SD renamed 2015 -> Oglala Lakota County",
    "Bedford (independent city) VA reverted to town 2013 -> Bedford County",
    "Dade County FL renamed 1997 -> Miami-Dade County",
    paste("Yellowstone National Park County MT abolished 1997, split between Park",
          "(30067, UIC 6, RURAL) and Gallatin (30031, UIC 4, small metro, NOT rural).",
          "Assigned to Park: the settled Montana portion of the park (Gardiner,",
          "Mammoth) lies in Park County. JUDGMENT CALL -- document it; it flips the",
          "rural flag for the affected institutions."),
    "South Boston (independent city) VA reverted to town 1995 -> Halifax County"
  )
)

## Connecticut is handled separately: Census replaced 8 counties with 9
## planning regions in 2022, which is a genuine many-to-many change, not a
## rename. Before building a crosswalk, check whether it matters at all --
## if no CT geography is rural in either vintage, the mismatch cannot affect
## the rural flag and a documented note is enough.
ct_matters <- function(u2013, u2024) {
  a <- u2013[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]
  b <- u2024[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]
  cat("\nConnecticut geographies classified rural:\n")
  cat("  2013 UIC (counties)        : ", a$rural, " of ", a$n, "\n", sep = "")
  cat("  2024 UIC (planning regions): ", b$rural, " of ", b$n, "\n", sep = "")
  if (a$rural == 0 && b$rural == 0) {
    cat("  -> No CT geography is rural under either vintage. The county/planning-\n")
    cat("     region mismatch cannot change any credit union's rural flag.\n")
    cat("     Document it and move on; no crosswalk needed.\n\n")
    invisible(FALSE)
  } else {
    cat("  -> CT has rural geography. Build the crosswalk from\n")
    cat("     https://www2.census.gov/geo/docs/reference/ct_change/\n\n")
    invisible(TRUE)
  }
}

## Map historical FIPS onto current geography before joining the 2024 UIC.
harmonize_fips <- function(x, fips_col = "fips", uic24 = NULL) {
  x <- copy(x)

  one <- FIPS_CHANGES[kind != "split"]   # rename / absorbed / resolved are all 1:1
  n_hit <- x[get(fips_col) %in% one$old_fips, .N]
  if (n_hit) x[one, on = setNames("old_fips", fips_col), (fips_col) := i.new_fips]
  message("Harmonized ", n_hit, " rows on renamed/absorbed FIPS")

  ## Splits have no 1:1 backward map. They only matter if the successors
  ## disagree on rural status -- check rather than assume.
  splits <- FIPS_CHANGES[kind == "split"]
  for (old in unique(splits$old_fips)) {
    n <- x[get(fips_col) == old, .N]
    if (!n) next
    succ <- splits[old_fips == old, new_fips]
    if (is.null(uic24)) {
      warning(n, " rows on split FIPS ", old, ". Pass uic24 to resolve.", call. = FALSE); next
    }
    r <- uic24[fips %in% succ, unique(rural)]
    if (length(r) == 1L) {
      x[get(fips_col) == old, (fips_col) := succ[1]]
      message(old, ": successors agree (rural = ", r, "); mapped to ", succ[1],
              " -- ", n, " rows, rural flag unaffected")
    } else {
      warning(old, " successors DISAGREE on rural status -- assign ", n,
              " rows by hand", call. = FALSE)
    }
  }
  x[]
}

## ---------------------------------------------------------------------------
## CONNECTICUT
## ---------------------------------------------------------------------------
## Census replaced CT's 8 counties with 9 planning regions in 2022. The 2024
## UIC uses planning regions; historical Call Report records carry the old
## county codes, so a naive join drops every Connecticut credit union.
##
## A county-to-planning-region crosswalk is many-to-many and would be a
## fabrication at the institution level. The defensible move is to check
## whether the mismatch can affect the rural flag at all: if no CT geography
## is rural under either vintage, assign rural = 0 and document it.

resolve_ct <- function(x, u2013, u2024, fips_col = "fips",
                       rural_cols = c("rural_2024", "rural_2013")) {
  a <- u2013[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]
  b <- u2024[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]
  cat("\n--- Connecticut resolution ---\n")
  cat("  rural geographies, 2013 UIC (counties)        : ", a$rural, " of ", a$n, "\n", sep = "")
  cat("  rural geographies, 2024 UIC (planning regions): ", b$rural, " of ", b$n, "\n", sep = "")

  n_ct <- x[substr(get(fips_col), 1, 2) == "09", .N]
  if (a$rural == 0 && b$rural == 0) {
    for (rc in intersect(rural_cols, names(x)))
      x[substr(get(fips_col), 1, 2) == "09" & is.na(get(rc)), (rc) := 0L]
    cat("  -> No CT geography is rural under either vintage, so the county/\n")
    cat("     planning-region mismatch cannot change any rural flag.\n")
    cat("     Assigned rural = 0 to ", n_ct, " Connecticut CU-quarters.\n", sep = "")
    cat("     DOCUMENT THIS in the technical appendix.\n\n")
  } else {
    cat("  -> CT HAS rural geography. A crosswalk is required; records left\n")
    cat("     unclassified. Source: https://www2.census.gov/geo/docs/reference/ct_change/\n\n")
  }
  x[]
}

## ---------------------------------------------------------------------------
## WHY DID RECORDS FAIL TO MATCH?
## ---------------------------------------------------------------------------
## Categorize before dropping. Systematic loss (a whole state) is a different
## problem from scattered loss, and only one of them is acceptable.

diagnose_unmatched <- function(x, fips_col = "fips", rural_col = "rural") {
  u <- x[is.na(get(rural_col))]
  if (!nrow(u)) { cat("\nNo unmatched CU-quarters.\n"); return(invisible(NULL)) }

  u[, reason := fifelse(substr(get(fips_col), 3, 5) == "000", "county code missing (000)",
                fifelse(substr(get(fips_col), 1, 2) == "09", "Connecticut (planning regions)",
                fifelse(as.integer(substr(get(fips_col), 1, 2)) > 56, "territory",
                        "unrecognized FIPS")))]
  tab <- u[, .(cu_quarters = .N, credit_unions = uniqueN(cu_number)), by = reason][order(-cu_quarters)]
  cat("\n--- Why records are unmatched ---\n"); print(tab)
  cat("\nWorst offenders:\n")
  print(u[, .(cu_quarters = .N, credit_unions = uniqueN(cu_number)),
          by = .(fips = get(fips_col), reason)][order(-cu_quarters)][1:12])
  cat("\n'county code missing' rows can often be recovered from zip_code_char5\n")
  cat("via the HUD USPS ZIP-to-county crosswalk. Territories need an explicit\n")
  cat("scope decision -- Puerto Rico carries UICs, the others do not.\n\n")
  invisible(tab)
}

## County-level reclassification, straight off the two UIC tables. This is the
## reclassification input for Q2 and needs no Call Report panel -- run it as
## soon as uic_setup() finishes.
rural_vintage_shift <- function(u2013, u2024, states_only = TRUE) {
  a <- u2013[, .(fips, uic_2013 = uic, rural_2013 = rural)]
  b <- u2024[, .(fips, uic_2024 = uic, rural_2024 = rural)]
  if (states_only) {
    a <- a[as.integer(substr(fips, 1, 2)) <= 56]
    b <- b[as.integer(substr(fips, 1, 2)) <= 56]
  }

  ## Report non-matches rather than dropping them silently. Connecticut is the
  ## expected offender: Census replaced its counties (09001-09015) with nine
  ## planning regions (09110-09190) in 2022, so the 2013 and 2024 files use
  ## different geography for that state entirely.
  only13 <- setdiff(a$fips, b$fips)
  only24 <- setdiff(b$fips, a$fips)
  if (length(only13) || length(only24)) {
    cat("\nFIPS present in only one vintage (excluded from the crosstab):\n")
    cat("  2013 only: ", length(only13),
        if (any(substr(only13, 1, 2) == "09")) "  (includes old Connecticut counties)" else "", "\n", sep = "")
    cat("  2024 only: ", length(only24),
        if (any(substr(only24, 1, 2) == "09")) "  (includes Connecticut planning regions)" else "", "\n", sep = "")
  }

  m <- merge(a, b, by = "fips")
  tab <- m[, .N, by = .(rural_2013, rural_2024)][order(-rural_2013, -rural_2024)]
  tab[, status := fifelse(rural_2013 == rural_2024, "stable",
                   fifelse(rural_2013 == 1L, "LOST rural status", "GAINED rural status"))]
  cat("\nRural status, 2013 UIC vs 2024 UIC (", nrow(m), " matched counties):\n", sep = "")
  print(tab)

  chg <- m[rural_2013 != rural_2024]
  cat("\n  net change in rural county count: ",
      sprintf("%+d", sum(m$rural_2024) - sum(m$rural_2013)), "\n", sep = "")
  cat("  counties that changed status    : ", nrow(chg), "\n", sep = "")
  cat("\n  Every one of these changed because OMB redelineated the county in\n")
  cat("  2023, NOT because ERS renumbered the codes -- the crosswalk is\n")
  cat("  rural-status preserving (see section 2). This is the reclassification\n")
  cat("  component of Q2: institutions in these counties change category with\n")
  cat("  no change in their business whatsoever.\n\n")

  invisible(list(crosstab = tab, changed = chg[order(fips)],
                 only_2013 = only13, only_2024 = only24))
}

compare_vintages <- function(dt) {
  ## Requires rural_2013 and rural_2024 both attached.
  stopifnot(all(c("rural_2013", "rural_2024") %in% names(dt)))
  cty <- unique(dt[, .(fips, rural_2013, rural_2024)])
  tab <- cty[, .N, by = .(rural_2013, rural_2024)][order(rural_2013, rural_2024)]
  tab[, status := fifelse(rural_2013 == rural_2024, "stable",
                   fifelse(rural_2013 == 1, "lost rural status", "gained rural status"))]
  print(tab)
  cat("\nAll status changes above are attributable to OMB's 2023 redelineation,\n",
      "not to the ERS code restructuring -- see section 2 of this file.\n")
  tab[]
}

## ---------------------------------------------------------------------------
## 7. KNOWN JOIN HAZARDS -- check each before trusting the merge
## ---------------------------------------------------------------------------
## (a) CONNECTICUT. Census replaced CT counties with nine planning regions in
##     2022 (FIPS 09110-09190). The 2024 UIC uses planning regions; NCUA
##     records for earlier quarters will carry the old county codes
##     (09001-09015). Without a crosswalk every CT credit union drops out.
##     Census crosswalk: https://www2.census.gov/geo/docs/reference/ct_change/
##
## (b) VIRGINIA. ERS combined nine independent cities with their surrounding
##     counties for UIC assignment but reports them separately, so both FIPS
##     appear and share a code. Should join cleanly; verify.
##
## (c) NCUA county_code. Confirm this is the FIPS county code and not a
##     sequential NCUA code. A match rate below ~95% is the tell. Fallback:
##     HUD USPS ZIP-to-county crosswalk on zip_code_char5, taking the
##     highest-residential-ratio county per ZIP.
##
## (d) TERRITORIES. Puerto Rico municipios carry UICs; other territories may
##     not. Decide explicitly whether they are in scope and document it.

CT_OLD_FIPS <- sprintf("09%03d", c(1,3,5,7,9,11,13,15))

check_join_hazards <- function(dt) {
  cat("\n--- join hazard check ---\n")
  ct <- dt[fips %in% CT_OLD_FIPS, .N]
  cat("CU-quarters on OLD Connecticut county FIPS:", ct,
      if (ct > 0) " <-- needs planning-region crosswalk\n" else "\n")
  terr <- dt[as.integer(substr(fips, 1, 2)) > 56, .N]
  cat("CU-quarters in territories (state FIPS > 56):", terr, "\n")
  cat("Distinct FIPS in panel:", uniqueN(dt$fips), "\n")
  invisible(NULL)
}

## ---------------------------------------------------------------------------
## 8. ONE-CALL SETUP
## ---------------------------------------------------------------------------
## Fetches both vintages, validates against the ERS published county counts,
## and hands back a list. This is the only thing you need to call.
##
##     source("rural_definition.R")
##     uic <- uic_setup()
##
## Then:  uic$u2024   uic$u2013

uic_setup <- function(cache_dir = "data/raw", vintages = c("2024", "2013")) {
  out <- list()
  for (v in vintages) {
    path <- fetch_uic(v, cache_dir = cache_dir)
    out[[paste0("u", v)]] <- load_uic(path, vintage = v)
  }
  if ("2024" %in% vintages) {
    cat("\nValidating 2024 UIC against ERS published county counts:\n")
    validate_uic(out$u2024)
  }
  cat("\nReady. Use uic$u2024 (primary) and uic$u2013 (prior vintage).\n\n")
  out
}

## ---------------------------------------------------------------------------
## 9. USAGE  --  NOTHING BELOW THIS LINE RUNS
## ---------------------------------------------------------------------------
## The `if (FALSE)` wrapper is deliberate: sourcing this file should define
## functions, not start downloading things. To actually run any of it, copy
## the lines to the console. Sourcing the file alone does nothing visible --
## that is expected, not a failure.

if (FALSE) {

  ## ---- minimum you need -----------------------------------------------
  uic <- uic_setup()

  ## ---- or step by step, if something goes wrong -------------------------
  uic_status()                                    # what is cached, and where
  uic24 <- load_uic(fetch_uic("2024"), "2024")
  uic13 <- load_uic(fetch_uic("2013"), "2013")
  validate_uic(uic24)

  ## ---- attach to the Call Report panel ----------------------------------
  dt <- build_fips(dt)
  check_join_hazards(dt)

  ## PRIMARY: "currently applicable" UICs = the 2024 vintage, applied to all
  ## years. Fixed-vintage is both the faithful reading of the statute's
  ## present-tense language and the right choice for trend series, since it
  ## isolates institutional change from map redrawing.
  dt <- attach_rural(dt, uic$u2024, label = "rural_2024")

  ## SECONDARY: prior vintage, for the reclassification component of Q2.
  dt <- attach_rural(dt, uic$u2013, label = "rural_2013")
  compare_vintages(dt)

  dt[, rural := rural_2024]
}
