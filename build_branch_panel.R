###############################################################################
# build_branch_panel.R
#
# Quarterly branch panel from NCUA "Credit Union Branch Information.txt",
# 2010Q3 - 2026Q1, with statutory rural classification per site.
#
# WHY THIS IS WORTH BUILDING
#   Every result so far classifies a credit union by its HEADQUARTERS county.
#   That cannot distinguish a metro-HQ institution with six rural branches from
#   a rural-HQ institution lending entirely in a metro area. This panel gives:
#
#     - a TIME-VARYING service-area measure (S2), not a snapshot
#     - branch OPENINGS and CLOSINGS via the stable SiteId
#     - county-level access: rural counties served from outside their borders
#     - the rural/urban flips that are real relocations, separated from the
#       ones that are just OMB redrawing county boundaries
#
# SOURCE
#   https://ncua.gov/analysis/credit-union-corporate-call-report-data/quarterly-data
#   Download the quarterly ZIPs into one folder. This reads the branch file out
#   of each ZIP directly -- no need to unpack them by hand.
#
# THE RULE THAT MATTERS
#   Rural status is assigned on a FIXED 2024 UIC vintage for every quarter, so
#   a site that changes rural status has actually MOVED. The reclassification
#   component is reported separately, never mixed into the movement series.
###############################################################################

library(data.table)
source("rural_definition.R")

## ---------------------------------------------------------------------------
## 0. DOWNLOAD THE QUARTERLY ARCHIVES FROM NCUA.GOV
## ---------------------------------------------------------------------------
## URLs taken directly from the quarterly data page. There is no single naming
## rule -- NCUA changed hosting conventions twice and left three one-offs --
## so the table below is explicit rather than inferred:
##
##   2016Q1 +      analysis/call-report-data-YYYY-MM.zip     (lowercase)
##   2015Q2-Q4     analysis/Call-Report-Data-YYYY-MM.zip     (title case)
##   <= 2015Q1     data-apps/QCRYYYYMM.zip
##   2013Q2        data-apps/5300Data0613Final.zip           (one-off)
##   2010Q2, Q4    data-apps/QCRYYYYMM.Zip                   (capital Z)
##
## The branch table starts Sep-2010, so 2010Q3 is the first usable quarter.
##
## SIZE. Each archive is tens of megabytes; 63 quarters runs to several GB.
## By default the branch file is extracted and the archive deleted, which
## leaves a few hundred MB. Set keep_zip = TRUE if you want the other tables
## (FS220 series, FOICU, ATM Locations) for later work.

ncua_zip_url <- function(year, month) {
  ym <- sprintf("%04d-%02d", year, month); ymc <- sprintf("%04d%02d", year, month)
  one_off <- c("2013-06" = "data-apps/5300Data0613Final.zip",
               "2010-06" = "data-apps/QCR201006.Zip",
               "2010-12" = "data-apps/QCR201012.Zip")
  tail <- if (ym %in% names(one_off)) one_off[[ym]]
          else if (year >= 2016) paste0("analysis/call-report-data-", ym, ".zip")
          else if (year == 2015 && month >= 6) paste0("analysis/Call-Report-Data-", ym, ".zip")
          else paste0("data-apps/QCR", ymc, ".zip")
  paste0("https://ncua.gov/files/publications/", tail)
}

quarter_seq <- function(from = c(2010, 3), to = c(2026, 1)) {
  q1 <- from[1] * 4L + from[2]; q2 <- to[1] * 4L + to[2]
  q  <- seq.int(q1, q2)
  data.table(qidx = q, year = q %/% 4L, quarter = q %% 4L)[quarter != 0L | TRUE][
    , `:=`(year = fifelse(quarter == 0L, year - 1L, year),
           quarter = fifelse(quarter == 0L, 4L, quarter))][]
}

.is_zip <- function(f) {
  if (!file.exists(f) || file.size(f) < 1000) return(FALSE)
  con <- file(f, "rb"); m <- readBin(con, "raw", 2); close(con)
  identical(as.character(m), c("50", "4b"))    # "PK"
}

download_ncua_quarters <- function(dest_dir, from = c(2010, 3), to = c(2026, 1),
                                   keep_zip = FALSE, overwrite = FALSE, pause = 1) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  if (.Platform$OS.type == "windows") options(download.file.method = "wininet")

  qs <- quarter_seq(from, to)
  cat("\n=== NCUA quarterly download: ", nrow(qs), " quarters (",
      qs[1, paste0(year, "Q", quarter)], " to ", qs[.N, paste0(year, "Q", quarter)], ") ===\n", sep = "")
  cat("Destination: ", normalizePath(dest_dir, mustWork = FALSE), "\n", sep = "")
  cat("Archives are kept: ", keep_zip, "\n\n", sep = "")

  log <- vector("list", nrow(qs))
  for (i in seq_len(nrow(qs))) {
    y <- qs$year[i]; m <- qs$quarter[i] * 3L
    tag <- sprintf("%dQ%d", y, qs$quarter[i])
    url <- ncua_zip_url(y, m)
    out_txt <- file.path(dest_dir, sprintf("Credit Union Branch Information_%s.txt", tag))
    out_zip <- file.path(dest_dir, sprintf("call-report-%s.zip", tag))

    if (file.exists(out_txt) && !overwrite) {
      cat(sprintf("  %-8s cached\n", tag))
      log[[i]] <- data.table(tag, url, status = "cached"); next
    }

    zf <- if (keep_zip) out_zip else tempfile(fileext = ".zip")
    ok <- FALSE; err <- NA_character_
    for (meth in c(getOption("download.file.method", "libcurl"), "libcurl", "curl")) {
      tryCatch({
        suppressWarnings(utils::download.file(url, zf, mode = "wb", method = meth, quiet = TRUE))
      }, error = function(e) err <<- conditionMessage(e))
      if (.is_zip(zf)) { ok <- TRUE; break }
    }
    if (!ok) {
      cat(sprintf("  %-8s FAILED  %s\n", tag, if (is.na(err)) "not a valid ZIP (404?)" else err))
      log[[i]] <- data.table(tag, url, status = "failed"); unlink(zf); next
    }

    inner <- grep(BRANCH_FILE_RX, utils::unzip(zf, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)
    if (!length(inner)) {
      cat(sprintf("  %-8s no branch file in archive\n", tag))
      log[[i]] <- data.table(tag, url, status = "no branch table")
      if (!keep_zip) unlink(zf); next
    }
    td <- tempfile(); dir.create(td)
    utils::unzip(zf, files = inner[1], exdir = td, junkpaths = TRUE)
    file.copy(file.path(td, basename(inner[1])), out_txt, overwrite = TRUE)
    unlink(td, recursive = TRUE); if (!keep_zip) unlink(zf)

    cat(sprintf("  %-8s ok  (%s KB)\n", tag, format(round(file.size(out_txt) / 1024), big.mark = ",")))
    log[[i]] <- data.table(tag, url, status = "downloaded")
    Sys.sleep(pause)   # be polite to a government server
  }

  res <- rbindlist(log)
  cat("\n--- summary ---\n"); print(res[, .N, by = status])
  bad <- res[status %in% c("failed", "no branch table")]
  if (nrow(bad)) {
    cat("\nQuarters needing manual download:\n")
    print(bad[, .(tag, url)])
    cat("\nIf these failed behind the agency proxy, try:\n")
    cat("  options(download.file.method = \"wininet\")\n")
    cat("or download the URLs in a browser and extract the branch file to:\n  ",
        normalizePath(dest_dir, mustWork = FALSE),
        "\nnaming it 'Credit Union Branch Information_YYYYQn.txt'.\n", sep = "")
  }
  invisible(res)
}

## ---------------------------------------------------------------------------
## 1. FIND THE SOURCE FILES
## ---------------------------------------------------------------------------
BRANCH_FILE_RX <- "credit.?union.?branch.?information.*\\.txt$"

scan_branch_sources <- function(dir) {
  zips <- list.files(dir, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  txts <- list.files(dir, pattern = BRANCH_FILE_RX, full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  cat("\n--- branch sources in ", dir, " ---\n", sep = "")
  cat("ZIP archives      : ", length(zips), "\n", sep = "")
  cat("loose branch files: ", length(txts), "\n", sep = "")
  if (length(zips)) {
    has <- vapply(zips, function(z)
      any(grepl(BRANCH_FILE_RX, utils::unzip(z, list = TRUE)$Name, ignore.case = TRUE)),
      logical(1))
    cat("ZIPs containing a branch file: ", sum(has), " of ", length(zips), "\n", sep = "")
    if (any(!has)) {
      cat("ZIPs WITHOUT one (pre-2010Q3 archives do not have it):\n")
      print(basename(zips[!has]))
    }
  }
  cat("Expected for 2010Q3-2026Q1: 63 quarters\n\n")
  invisible(list(zips = zips, txts = txts))
}

## ---------------------------------------------------------------------------
## 2. READ ONE FILE
## ---------------------------------------------------------------------------
read_branch_file <- function(path, inside_zip = NULL) {
  con <- if (is.null(inside_zip)) path else unz(path, inside_zip)
  x <- tryCatch(fread(if (is.null(inside_zip)) path else
                        paste(readLines(con, warn = FALSE), collapse = "\n"),
                      colClasses = "character", showProgress = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  setnames(x, tolower(gsub("[^A-Za-z0-9]+", "_", names(x))))

  need <- c("cu_number","cycle_date","siteid","sitetypename","mainoffice",
            "physicaladdresscity","physicaladdressstatecode",
            "physicaladdresspostalcode","physicaladdresscountry")
  cty <- grep("^physicaladdresscountyname", names(x), value = TRUE)[1]
  if (!all(need %in% names(x)) || is.na(cty)) {
    warning("Unexpected layout in ", basename(path), " -- columns: ",
            paste(names(x), collapse = ", "), call. = FALSE)
    return(NULL)
  }

  d <- x[, .(
    cu_number  = suppressWarnings(as.integer(cu_number)),
    site_id    = suppressWarnings(as.integer(siteid)),
    cycle_date = as.Date(sub(" .*$", "", cycle_date), format = "%m/%d/%Y"),
    site_type  = sitetypename,
    main_office = toupper(substr(mainoffice, 1, 1)) == "Y",
    city   = physicaladdresscity,
    state  = physicaladdressstatecode,
    zip5   = sprintf("%05d", suppressWarnings(as.integer(substr(physicaladdresspostalcode, 1, 5)))),
    county_name = trimws(get(cty)),
    country = physicaladdresscountry,
    atm       = get0("atm", ifnotfound = NA) %||% NA,
    memberserv = get0("memberservices", ifnotfound = NA) %||% NA)]

  ## flag columns exist in most vintages but not all
  for (f in c("atm","driveThru","memberservices","shrd_serv_cntr_net")) {
    fl <- grep(paste0("^", tolower(f), "$"), names(x), value = TRUE)
    if (length(fl)) d[, (tolower(f)) := as.integer(x[[fl]])]
  }

  d[, `:=`(year    = as.integer(format(cycle_date, "%Y")),
           quarter = as.integer((as.integer(format(cycle_date, "%m")) - 1) %/% 3 + 1))]
  d[, qidx := year * 4L + quarter]
  d[!is.na(cu_number) & !is.na(site_id)]
}
`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------------
## 3. BUILD THE STACKED PANEL
## ---------------------------------------------------------------------------
build_branch_panel <- function(dir, cache = file.path(dir, "_branch_panel.rds"),
                               refresh = FALSE) {
  if (file.exists(cache) && !refresh) {
    message("Reading cached branch panel: ", cache); return(readRDS(cache))
  }
  src <- scan_branch_sources(dir)
  out <- list()

  for (z in src$zips) {
    inner <- grep(BRANCH_FILE_RX, utils::unzip(z, list = TRUE)$Name,
                  value = TRUE, ignore.case = TRUE)
    if (!length(inner)) next
    tmp <- tempfile(fileext = ".txt")
    utils::unzip(z, files = inner[1], exdir = dirname(tmp), junkpaths = TRUE)
    f <- file.path(dirname(tmp), basename(inner[1]))
    d <- read_branch_file(f); unlink(f)
    if (!is.null(d)) out[[z]] <- d
    message("  ", basename(z), ": ", if (is.null(d)) "SKIPPED" else
              paste0(nrow(d), " sites, ", d[1, year], "Q", d[1, quarter]))
  }
  for (t in src$txts) {
    d <- read_branch_file(t)
    if (!is.null(d)) out[[t]] <- d
    message("  ", basename(t), ": ", if (is.null(d)) "SKIPPED" else
              paste0(nrow(d), " sites, ", d[1, year], "Q", d[1, quarter]))
  }
  if (!length(out)) stop("No branch files could be read from ", dir, call. = FALSE)

  b <- rbindlist(out, fill = TRUE, use.names = TRUE)
  b <- unique(b, by = c("cu_number", "site_id", "qidx"))
  setorder(b, cu_number, site_id, qidx)

  qs <- b[, sort(unique(qidx))]
  cat("\nPanel: ", nrow(b), " site-quarters | ", uniqueN(b$cu_number), " credit unions | ",
      length(qs), " quarters (", b[qidx == min(qs), paste0(year[1], "Q", quarter[1])],
      " to ", b[qidx == max(qs), paste0(year[1], "Q", quarter[1])], ")\n", sep = "")
  gaps <- setdiff(seq(min(qs), max(qs)), qs)
  if (length(gaps)) cat("MISSING quarters: ", paste(gaps %/% 4, "Q", gaps %% 4, sep = "", collapse = ", "),
                        "\n  Gaps break the open/close event logic -- fill them before trusting events.\n", sep = "")

  saveRDS(b, cache); message("Cached to ", cache)
  b[]
}

## ---------------------------------------------------------------------------
## 4. COUNTY NAME -> FIPS, via the Stata crosswalk
## ---------------------------------------------------------------------------
## fips_national_final_withpostCensus2000updates.dta carries, for every county:
## the FIPS code and SEVERAL name variants (countyname, countyname2, ...
## countyname5), running from the full official form ("Prince of Wales-Hyder
## Census Area") down to a stripped form ("Prince Of Wales Hyder"). That range
## is exactly what NCUA's county field looks like across vintages.
##
## Two reasons this beats hand-written normalization:
##   1. It is authoritative rather than inferred -- defensible in the appendix.
##   2. "withpostCensus2000updates" means it carries HISTORICAL FIPS. A 2012
##      branch file says "Shannon" (SD) and "Wade Hampton" (AK); no current
##      reference has those. The crosswalk resolves them to the old codes, and
##      harmonize_fips() then maps those forward to Oglala Lakota and Kusilvak.
##
## THE AMBIGUITY RULE. Stripping suffixes collapses distinctions: Virginia's
## "Franklin County" and "Franklin city" both become "Franklin". Variants are
## therefore tried MOST SPECIFIC FIRST, and any key mapping to more than one
## FIPS within a state is refused at that level rather than resolved by
## guessing. This is the treatment variable; it is never assigned on a coin flip.

.norm_nm <- function(x) {
  s <- iconv(as.character(x), to = "ASCII//TRANSLIT"); s <- tolower(s)
  s <- gsub("[-.'`,]", " ", s)
  s <- gsub("\\s+", " ", trimws(s))
  s
}
## keep the independent-city distinction, which is the one that actually matters
.city_key <- function(x) {
  s <- .norm_nm(x)
  city <- grepl("\\bcity\\b\\s*$", s) & !grepl("city and borough", s)
  base <- gsub("\\b(county|parish|borough|census area|municipality|municipio|planning region|city and borough)\\b", " ", s)
  base <- gsub("\\bcity\\b\\s*$", "", base)
  base <- gsub("\\bst\\b", "saint", base); base <- gsub("\\bste\\b", "sainte", base)
  paste0(gsub("\\s+", " ", trimws(base)), fifelse(city, "|city", "|county"))
}

county_xwalk_stata <- function(path, verbose = TRUE) {
  if (!requireNamespace("haven", quietly = TRUE))
    stop("Reading the .dta crosswalk needs haven.", call. = FALSE)
  x <- setDT(haven::zap_labels(haven::read_dta(path)))
  setnames(x, tolower(gsub("[^A-Za-z0-9]+", "_", names(x))))

  ## FIPS: prefer a ready 5-digit statecty field, else build it
  fp <- grep("^statecty", names(x), value = TRUE)
  fp <- fp[vapply(fp, function(c) mean(grepl("^[0-9]{5}$",
              sprintf("%05s", trimws(as.character(x[[c]]))))) > .9, logical(1))]
  if (length(fp)) {
    x[, fips := sprintf("%05d", suppressWarnings(as.integer(get(fp[1]))))]
  } else {
    sc <- grep("^state_code$|^state_c", names(x), value = TRUE)[1]
    cc <- grep("^county_co", names(x), value = TRUE)[1]
    if (is.na(sc) || is.na(cc)) stop("Cannot locate FIPS columns in the crosswalk.", call. = FALSE)
    x[, fips := sprintf("%02d%03d", as.integer(get(sc)), as.integer(get(cc)))]
  }
  st <- grep("^state$", names(x), value = TRUE)[1]
  if (is.na(st)) stop("Cannot locate a state postal column in the crosswalk.", call. = FALSE)

  nmc <- grep("^countyname[0-9]*$", names(x), value = TRUE)
  nmc <- nmc[order(as.integer(sub("^countyname", "", nmc)), na.last = FALSE)]
  if (!length(nmc)) stop("No countyname* columns found.", call. = FALSE)

  long <- rbindlist(lapply(seq_along(nmc), function(i)
    data.table(level = i, variant = nmc[i],
               state = as.character(x[[st]]),
               name  = as.character(x[[nmc[i]]]),
               fips  = x$fips)))
  long <- long[nzchar(trimws(name)) & !is.na(fips)]
  long[, key := .city_key(name)]

  ## refuse any key that is ambiguous within its state and level
  amb <- long[, .(n_fips = uniqueN(fips)), by = .(level, state, key)][n_fips > 1L]
  long <- long[!amb, on = .(level, state, key)]
  long <- unique(long, by = c("level", "state", "key"))

  if (verbose) {
    cat("\n--- county crosswalk (", basename(path), ") ---\n", sep = "")
    cat("counties: ", uniqueN(x$fips), " | name variants: ", length(nmc), "\n", sep = "")
    print(long[, .(usable_keys = .N), by = .(level, variant)][order(level)])
    if (nrow(amb)) {
      cat("\nAmbiguous keys refused (", nrow(amb), " state-level-key combos). Top:\n", sep = "")
      print(head(amb[order(level, state, key)], 10))
      cat("These are resolved at a more specific variant level, or not at all.\n")
    }
  }
  setattr(long, "n_levels", length(nmc))
  long[]
}

## Fill NA fips in place. Written explicitly because
##   b[cond][xw, on = ..., fips := i.fips]
## assigns into a temporary copy and silently does nothing.
.fill_fips <- function(b, xw, key_col, label) {
  idx <- b[, which(is.na(fips) & !foreign & !territory & nzchar(county_name))]
  if (!length(idx)) return(0L)
  got <- xw[b[idx, .(state, k = get(key_col))], on = .(state, key = k), x.fips]
  n <- sum(!is.na(got))
  if (n) {
    b[idx, fips := got]
    b[idx[!is.na(got)], fips_src := label]
  }
  n
}

## ---------------------------------------------------------------------------
## 5. ATTACH RURAL STATUS TO EVERY SITE
## ---------------------------------------------------------------------------
attach_site_rural <- function(b, uic24, uic13 = NULL, xw, zcta_file = NULL) {
  b <- copy(b)
  b[, `:=`(fips = NA_character_, fips_src = NA_character_,
           foreign   = country != "United States" & nzchar(country),
           territory = state %in% TERRITORY_STATES,
           ckey      = .city_key(county_name))]
  n_cand <- b[!foreign & !territory & nzchar(county_name), .N]
  cat("\n--- site county matching ---\nCandidate site-quarters: ", n_cand, "\n", sep = "")

  ## Most specific variant first: an exact official-name match is worth more
  ## than a stripped-form match, and resolves the city/county collisions.
  for (lv in sort(unique(xw$level))) {
    got <- .fill_fips(b, xw[level == lv], "ckey", paste0("crosswalk: ", xw[level == lv, variant[1]]))
    cat("  variant ", lv, " (", xw[level == lv, variant[1]], "): +", got, "\n", sep = "")
  }

  ## Connecticut: the file already uses PLANNING REGIONS but abbreviates them
  ## ("NW Hills", "Naugatuck Vly"), and a post-2000 crosswalk predates them.
  ## Matched by keyword against the ERS names so it verifies against the file
  ## rather than trusting hard-coded region FIPS.
  if (b[is.na(fips) & state == "CT", .N]) {
    ctx <- ct_xwalk_ers(uic24)
    if (!is.null(ctx)) {
      idx <- b[, which(is.na(fips) & state == "CT")]
      nm  <- .norm_nm(b$county_name[idx])
      hit <- ctx$fips[match(nm, ctx$ncua_name)]
      b[idx, `:=`(fips = hit, fips_src = fifelse(is.na(hit), NA_character_, "CT planning region"))]
      cat("  CT planning regions: +", sum(!is.na(hit)), "\n", sep = "")
    }
  }

  ## ZIP fallback, unambiguous ZIPs only
  if (!is.null(zcta_file) && b[is.na(fips) & !foreign & !territory, .N]) {
    z <- fread(zcta_file, colClasses = "character"); setnames(z, tolower(names(z)))
    zc <- grep("zcta", names(z), value = TRUE)[1]
    cc <- grep("county.*geoid|geoid.*county", names(z), value = TRUE)[1]
    if (!is.na(zc) && !is.na(cc)) {
      xz <- unique(z[, .(zip5 = sprintf("%05d", as.integer(get(zc))),
                         fips = sprintf("%05d", as.integer(get(cc))))])
      xz <- merge(xz, uic24[, .(fips, rural)], by = "fips")
      ok  <- xz[, .(n = uniqueN(rural), fips = fips[1]), by = zip5][n == 1L]
      idx <- b[, which(is.na(fips) & !foreign & !territory)]
      got <- ok$fips[match(b$zip5[idx], ok$zip5)]
      b[idx, fips := got]; b[idx[!is.na(got)], fips_src := "ZIP crosswalk"]
      cat("  ZIP crosswalk: +", sum(!is.na(got)), "\n", sep = "")
    }
  }

  ## HISTORICAL -> CURRENT. The crosswalk returns the FIPS in force at the
  ## time, so early quarters carry Shannon SD, Wade Hampton AK, Dade FL. Map
  ## them forward BEFORE classifying, or they drop out.
  b <- harmonize_fips(b, uic24 = uic24)

  ## Rural on a FIXED 2024 vintage, so a flip means the site actually moved
  b[uic24[, .(fips, r = rural)], on = "fips", rural_site := i.r]
  if (!is.null(uic13)) b[uic13[, .(fips, r = rural)], on = "fips", rural_site_2013 := i.r]
  b[, ckey := NULL]

  cat("\nMatch by source:\n")
  print(b[!foreign & !territory, .(site_quarters = .N), by = fips_src][order(-site_quarters)])
  unm <- b[!foreign & !territory & is.na(rural_site)]
  cat("\nUnmatched: ", nrow(unm), " site-quarters (",
      round(100 * nrow(unm) / max(1, b[!foreign & !territory, .N]), 2), "%)\n", sep = "")
  if (nrow(unm)) {
    cat("Worst remaining name/state pairs:\n")
    print(head(unm[, .N, by = .(state, county_name)][order(-N)], 15))
  }
  cat("\nExcluded by design: ", b[(foreign), .N], " foreign site-quarters, ",
      b[(territory), .N], " territory site-quarters (no UIC).\n", sep = "")
  b[]
}

TERRITORY_STATES <- c("GU","VI","AS","MP","FM","MH","PW")

## Connecticut planning regions, matched to ERS names by keyword.
CT_PATTERNS <- c("capitol" = "capitol", "greater bridgeport" = "greater bridgeport",
                 "lower ct river vly" = "lower connecticut river", "naugatuck vly" = "naugatuck",
                 "northeastern ct" = "northeastern", "nw hills" = "northwest hills",
                 "south central ct" = "south central", "southeastern ct" = "southeastern",
                 "western ct" = "western")

ct_xwalk_ers <- function(uic24, ers_path = fetch_uic("2024")) {
  z <- fread(ers_path, colClasses = "character")
  setnames(z, tolower(gsub("[^a-z0-9]+", "_", tolower(names(z)))))
  fp <- grep("^fips", names(z), value = TRUE)[1]
  st <- grep("^state$", names(z), value = TRUE)[1]
  cn <- grep("county", names(z), value = TRUE)[1]
  ct <- unique(z[get(st) == "CT", .(fips = sprintf("%05d", as.integer(get(fp))),
                                    county_name = get(cn))])
  if (!nrow(ct)) { warning("No Connecticut rows in the ERS file.", call. = FALSE); return(NULL) }
  out <- rbindlist(lapply(names(CT_PATTERNS), function(k) {
    hit <- ct[grepl(CT_PATTERNS[[k]], tolower(county_name))]
    if (nrow(hit) != 1L) {
      warning("CT '", k, "' matched ", nrow(hit), " ERS rows -- resolve by hand.", call. = FALSE)
      return(NULL)
    }
    data.table(ncua_name = k, fips = hit$fips, ers_name = hit$county_name)
  }))
  cat("\nConnecticut crosswalk:\n"); print(out)
  out
}

## ---------------------------------------------------------------------------
## 6. THE CREDIT UNION x QUARTER TIME SERIES
## ---------------------------------------------------------------------------
cu_branch_timeseries <- function(b) {
  ts <- b[!foreign & !territory, .(
    n_sites        = .N,
    n_hq           = sum(main_office, na.rm = TRUE),
    n_branches     = sum(!main_office, na.rm = TRUE),
    n_classified   = sum(!is.na(rural_site)),
    n_rural_sites  = sum(rural_site == 1L, na.rm = TRUE),
    n_urban_sites  = sum(rural_site == 0L, na.rm = TRUE),
    hq_rural       = as.integer(any(main_office & rural_site == 1L, na.rm = TRUE)),
    n_atm          = if ("atm" %in% names(b)) sum(atm == 1L, na.rm = TRUE) else NA_integer_
  ), by = .(cu_number, year, quarter, qidx)]

  ts[, `:=`(rural_site_share = fifelse(n_classified > 0, n_rural_sites / n_classified, NA_real_))]
  ts[, `:=`(S2_majority_rural = as.integer(rural_site_share >= 0.5),
            S2_any_rural      = as.integer(n_rural_sites > 0))]
  setorder(ts, cu_number, qidx)

  ## quarter-over-quarter movement
  ts[, `:=`(d_sites = n_sites - shift(n_sites),
            d_rural = n_rural_sites - shift(n_rural_sites),
            d_urban = n_urban_sites - shift(n_urban_sites)), by = cu_number]
  ts[]
}

## ---------------------------------------------------------------------------
## 7. SITE-LEVEL EVENTS -- openings, closings, and genuine relocations
## ---------------------------------------------------------------------------
## SiteId is stable across quarters, which is what makes this possible. A site
## that appears is an opening; one that disappears is a closing; one whose
## county changes has MOVED -- because the rural vintage is held fixed.
branch_events <- function(b) {
  s <- b[!foreign & !territory, .(cu_number, site_id, qidx, year, quarter, fips, rural_site)]
  setorder(s, cu_number, site_id, qidx)
  allq <- sort(unique(s$qidx))

  life <- s[, .(first_q = min(qidx), last_q = max(qidx), n_q = .N,
                first_fips = fips[which.min(qidx)], last_fips = fips[which.max(qidx)],
                first_rural = rural_site[which.min(qidx)],
                last_rural  = rural_site[which.max(qidx)]),
            by = .(cu_number, site_id)]

  life[, `:=`(opened = first_q > min(allq),
              closed = last_q  < max(allq),
              moved_county = !is.na(first_fips) & !is.na(last_fips) & first_fips != last_fips,
              flipped = !is.na(first_rural) & !is.na(last_rural) & first_rural != last_rural)]

  cat("\n=== Site lifecycle, ", length(allq), " quarters ===\n", sep = "")
  print(life[, .(sites = .N,
                 opened = sum(opened), closed = sum(closed),
                 moved_county = sum(moved_county),
                 changed_rural_status = sum(flipped))])

  cat("\nRural-status flips (fixed 2024 vintage => these are real relocations):\n")
  print(life[flipped == TRUE, .N,
             by = .(direction = fifelse(first_rural == 1L, "rural -> urban", "urban -> rural"))])

  ## openings and closings by quarter and rural status
  op <- s[life[opened == TRUE], on = .(cu_number, site_id, qidx = first_q),
          .(openings = .N), by = .(qidx, rural = rural_site)]
  cl <- s[life[closed == TRUE], on = .(cu_number, site_id, qidx = last_q),
          .(closings = .N), by = .(qidx, rural = rural_site)]
  flow <- merge(op, cl, by = c("qidx","rural"), all = TRUE)
  setnafill(flow, fill = 0, cols = c("openings","closings"))
  flow[, `:=`(year = qidx %/% 4L, quarter = qidx %% 4L, net = openings - closings)]

  cat("\nBranch openings and closings by year and rural status:\n")
  print(dcast(flow[, .(openings = sum(openings), closings = sum(closings), net = sum(net)),
                   by = .(year, rural)], year ~ rural, value.var = c("openings","closings","net")))
  cat("\nThis is the depository-access story Q8 needs: whether rural counties\n")
  cat("are losing credit union presence, and whether closings accelerated.\n")

  invisible(list(lifecycle = life, flows = flow))
}

## ---------------------------------------------------------------------------
## 8. RECLASSIFICATION COMPONENT -- kept separate, never mixed in
## ---------------------------------------------------------------------------
site_reclassification <- function(b) {
  if (!"rural_site_2013" %in% names(b)) {
    message("rural_site_2013 not attached; pass uic13 to attach_site_rural()."); return(invisible(NULL))
  }
  cur <- b[qidx == max(qidx) & !foreign & !territory & !is.na(rural_site) & !is.na(rural_site_2013)]
  tab <- cur[, .(sites = .N, credit_unions = uniqueN(cu_number)),
             by = .(rural_site_2013, rural_site)]
  tab[, status := fifelse(rural_site_2013 == rural_site, "stable",
                   fifelse(rural_site_2013 == 1L, "lost rural status", "gained rural status"))]
  cat("\n=== Sites whose rural status depends only on UIC vintage ===\n"); print(tab)
  cat("These are NOT movements. Report them separately from branch_events().\n")
  invisible(tab)
}

## ---------------------------------------------------------------------------
## 9. MERGE WITH THE CALL REPORT PANEL
## ---------------------------------------------------------------------------
merge_branch_callreport <- function(cr, ts) {
  out <- merge(cr, ts[, .(cu_number, qidx, n_sites, n_hq, n_branches,
                          n_rural_sites, n_urban_sites, rural_site_share,
                          S2_majority_rural, S2_any_rural, hq_rural)],
               by = c("cu_number", "qidx"), all.x = TRUE)
  cov <- out[, .(pct_with_branch_data = round(100 * mean(!is.na(n_sites)), 1)), by = year][order(year)]
  cat("\nBranch coverage of the Call Report panel, by year:\n"); print(cov)

  d <- out[!is.na(S2_majority_rural) & !is.na(rural)]
  if (nrow(d)) {
    cat("\n=== S1 (HQ county) vs S2 (majority of sites) ===\n")
    print(d[, .N, by = .(S1_rural = rural, S2_majority_rural)][order(-S1_rural, -S2_majority_rural)])
    cat("Agreement: ", round(100 * d[, mean(rural == S2_majority_rural)], 2), "%\n", sep = "")
    cat("\nAbove ~95%: the HQ-based findings stand, S2 becomes a robustness row.\n")
    cat("Materially below: re-run E1-E14 on S2 before publishing anything\n")
    cat("about rural performance.\n")
  }
  out[]
}

## ---------------------------------------------------------------------------
## 10. USAGE
## ---------------------------------------------------------------------------
if (FALSE) {
  BRANCH_DIR <- file.path(PANEL_DIR, "branch_files")

  ## One-time: pull every quarterly archive and keep just the branch table.
  ## Re-running skips anything already present, so it resumes after a failure.
  download_ncua_quarters(BRANCH_DIR, from = c(2010, 3), to = c(2026, 1))

  scan_branch_sources(BRANCH_DIR)
  b   <- build_branch_panel(BRANCH_DIR)
  uic <- uic_setup(cache_dir = CACHE_DIR)
  xw  <- county_xwalk_stata(file.path(DATA_DIR, "fips_national_final_withpostCensus2000updates.dta"))
  b   <- attach_site_rural(b, uic$u2024, uic$u2013, xw = xw)  # add zcta_file= for the fallback

  ts  <- cu_branch_timeseries(b)
  ev  <- branch_events(b)
  site_reclassification(b)

  cr2 <- merge_branch_callreport(cr, ts)              # cr from panel_prep.R

  saveRDS(ts, file.path(OUT_DIR, "cu_branch_timeseries.rds"))
  fwrite(ev$flows, file.path(OUT_DIR, "branch_openings_closings.csv"))
}
