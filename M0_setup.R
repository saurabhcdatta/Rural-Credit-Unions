###############################################################################
# M0_setup.R  --  MASTER DATASET BUILD, step 0 of 5
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# THE IDEA
#   One chat builds the master dataset. Every research-question chat loads it
#   through rq_load.R and never touches raw data. If a number reaches Congress,
#   it traces to one frozen, versioned, hash-checked master build.
#
# THE CHECKPOINT DOCTRINE
#   Every script in this series ends in a block of chk() calls. A chk() that
#   fails STOPS the build. Nothing downstream can run on a failed checkpoint.
#   Four levels, in order of appearance:
#     L1 input integrity   -- files exist, sizes, hashes, expected columns
#     L2 structure         -- keys unique, no NA keys, counts in range
#     L3 content           -- values in range, flags coded as expected, anchors
#     L4 reconciliation    -- identities that must hold across tables
#   A checkpoint that cannot be made to pass is a finding, not an obstacle:
#   document it in the handoff and mark it with chk(..., soft = TRUE).
#
# RUN ORDER   M0 -> M1 -> M2 -> M3 -> M4 -> M5.  Block by block, in one session.
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(haven) })

## ---- M0.1  paths ------------------------------------------------------------
ROOT       <- "C:/Users/sdatta/OneDrive - NCUA/Rural_CUs"
CODE_DIR   <- file.path(ROOT, "code", "master")
DATA_DIR   <- file.path(ROOT, "data")
CACHE_DIR  <- file.path(DATA_DIR, "raw")
BRANCH_DIR <- file.path(DATA_DIR, "branch_files")
MASTER_DIR <- file.path(DATA_DIR, "master")          # frozen builds live here
OUT_DIR    <- file.path(ROOT, "output")

XWALK_FILE <- file.path(DATA_DIR, "fips_national_final_withpostCensus2000updates.dta")
PANEL_FILE <- list.files(DATA_DIR, "^OCE_CallReport_.*\\.dta$", full.names = TRUE)[1]

for (d in c(CACHE_DIR, BRANCH_DIR, MASTER_DIR, OUT_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

BUILD_VERSION <- format(Sys.Date(), "master_v%Y-%m-%d")
BUILD_DIR     <- file.path(MASTER_DIR, BUILD_VERSION)
dir.create(BUILD_DIR, showWarnings = FALSE)
cat("Build version:", BUILD_VERSION, "\n")

## ---- M0.2  packages: verify, never install ----------------------------------
NEED <- c("data.table", "haven", "readxl")
miss <- NEED[!vapply(NEED, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Packages not available on this machine: ", paste(miss, collapse = ", "))

## Downloads: wininet is gone in R 4.6.0. curl uses the Windows cert store.
if (.Platform$OS.type == "windows") options(download.file.method = "curl")
## Binary-safe check for a proxy block page masquerading as a download.
## A ZIP starts with the bytes "PK"; anything else is read as text with NULs stripped.
looks_like_html <- function(f) {
  b <- readBin(f, "raw", 400L)
  if (length(b) >= 2L && b[1] == as.raw(0x50) && b[2] == as.raw(0x4b)) return(FALSE)   # ZIP
  b[b == as.raw(0)] <- as.raw(32)
  txt <- tolower(iconv(rawToChar(b), from = "", to = "ASCII", sub = ""))
  grepl("<html|<!doctype", txt)
}
dl <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(invisible(dest))
  for (m in c("curl", "libcurl", "auto")) {
    ok <- tryCatch({ download.file(url, dest, mode = "wb", method = m, quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 0) {
      if (looks_like_html(dest)) { unlink(dest); next }
      return(invisible(dest))
    }
  }
  stop("Could not download ", url, "\nOpen it in a browser and save as ", dest)
}

## ---- M0.3  the checkpoint engine --------------------------------------------
## chk(cond, id, msg, soft = FALSE)
##   cond : a single TRUE/FALSE
##   id   : "L2-CR-03" style -- level, table, number. Stable across builds.
##   msg  : what is being asserted, in words a reviewer can read
##   soft : TRUE logs a WARN and continues; FALSE stops the build
CHK_LOG <- data.table(id = character(), level = character(), table = character(),
                      result = character(), message = character(), value = character(),
                      time = character())

chk <- function(cond, id, msg, value = NA, soft = FALSE) {
  ok <- isTRUE(cond)
  res <- if (ok) "PASS" else if (soft) "WARN" else "FAIL"
  parts <- strsplit(id, "-")[[1]]
  CHK_LOG <<- rbind(CHK_LOG, data.table(id = id, level = parts[1], table = parts[2],
                                        result = res, message = msg,
                                        value = as.character(value), time = format(Sys.time(), "%H:%M:%S")))
  cat(sprintf("  [%s] %-10s %s%s\n", res, id, msg, if (!is.na(value)) paste0("  = ", value) else ""))
  if (!ok && !soft) stop("CHECKPOINT FAILED: ", id, " -- ", msg, call. = FALSE)
  invisible(ok)
}
chk_section <- function(title) cat("\n== CHECKPOINTS:", title, "==\n")
chk_summary <- function() {
  s <- CHK_LOG[, .N, by = result]
  cat("\nCheckpoint summary:\n"); print(s)
  if (CHK_LOG[result == "FAIL", .N] > 0) stop("Build has FAILED checkpoints.")
  invisible(s)
}

## file identity, recorded in the manifest and re-verified by every RQ chat
md5 <- function(f) unname(tools::md5sum(f))

## ---- M0.4  statutory rural code sets ----------------------------------------
## 12 CFR 1026.35(b)(2)(iv)(A)(1): a county neither in an MSA nor in a
## micropolitan area adjacent to an MSA. In Urban Influence Code terms:
RURAL_2024 <- c(3L, 6L, 7L, 8L, 9L)                       # 9-category scheme
RURAL_2013 <- c(4L, 6L, 7L, 8L, 9L, 10L, 11L, 12L)        # 12-category scheme

## Adjacency class inside the rural set (for Q12). 2024 codes:
##   3 = noncore adjacent to LARGE metro; 6 = noncore adjacent to SMALL metro;
##   7 = micropolitan NOT adjacent; 8/9 = noncore not adjacent to a metro.
ADJ_CLASS_2024 <- c(`3` = "adjacent to large metro", `6` = "adjacent to small metro",
                    `7` = "remote micropolitan", `8` = "remote", `9` = "remote")

## ERS published county counts by 2024 code (50 states + DC). Must match exactly.
ERS_BENCH_2024 <- data.table(uic = 1:9, expected = c(443L, 130L, 154L, 743L, 272L, 490L, 256L, 125L, 531L))
ANCHOR_RURAL_COUNTIES <- 1556L

## ---- M0.5  geography helpers (from 1_setup.R, unchanged logic) --------------
norm_nm <- function(x) {
  s <- as.character(x)
  s <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1\u00fc\u00c1\u00c9\u00cd\u00d3\u00da\u00d1\u00dc", "aeiounuAEIOUNU", s)
  s <- tolower(s); s <- gsub("[-.'`,]", " ", s); gsub("\\s+", " ", trimws(s))
}
city_key <- function(x) {
  s    <- norm_nm(x)
  city <- grepl("\\bcity\\b\\s*$", s) & !grepl("city and borough", s)
  b    <- gsub("\\b(county|parish|borough|census area|municipality|municipio|planning region|city and borough)\\b", " ", s)
  b    <- gsub("\\bcity\\b\\s*$", "", b)
  b    <- gsub("\\bst\\b", "saint", b); b <- gsub("\\bste\\b", "sainte", b)
  paste0(gsub("\\s+", " ", trimws(b)), ifelse(city, "|city", "|county"))
}

FIPS_FIX <- data.table(
  old  = c("02261","02270","46113","51515","12025","30113","51780"),
  new  = c("02063","02158","46102","51019","12086","30067","51083"),
  note = c("Valdez-Cordova AK dissolved 2019 -> Chugach",
           "Wade Hampton AK renamed 2015 -> Kusilvak",
           "Shannon SD renamed 2015 -> Oglala Lakota",
           "Bedford city VA reverted to town 2013 -> Bedford County",
           "Dade FL renamed 1997 -> Miami-Dade",
           "Yellowstone NP County MT abolished 1997 -> Park (JUDGMENT, documented)",
           "South Boston city VA reverted to town 1995 -> Halifax County"))
TERRITORIES <- c("GU","VI","AS","MP","FM","MH","PW")

## quarter index: year*4 + quarter. Label with q_lab(); never with %/% 4.
q_year <- function(q) (q - 1L) %/% 4L
q_qtr  <- function(q) ((q - 1L) %% 4L) + 1L
q_lab  <- function(q) sprintf("%dQ%d", q_year(q), q_qtr(q))

## ---- M0.6  L1 checkpoints on the two core inputs ----------------------------
chk_section("M0 inputs")
chk(file.exists(XWALK_FILE), "L1-IN-01", "Stata FIPS crosswalk present", basename(XWALK_FILE))
chk(!is.na(PANEL_FILE) && file.exists(PANEL_FILE), "L1-IN-02", "Call Report panel present", basename(PANEL_FILE))
chk(file.size(PANEL_FILE) > 3e9, "L1-IN-03", "Call Report panel is the full file (> 3 GB)", round(file.size(PANEL_FILE)/1e9, 2))
chk(q_lab(2013L*4L+1L) == "2013Q1" && q_lab(2013L*4L+4L) == "2013Q4", "L1-IN-04", "quarter labelling handles Q4 correctly")
chk(city_key("Franklin city") != city_key("Franklin County"), "L1-IN-05", "county-name key keeps VA independent cities distinct")

INPUT_MANIFEST <- data.table(file = c(basename(XWALK_FILE), basename(PANEL_FILE)),
                             bytes = c(file.size(XWALK_FILE), file.size(PANEL_FILE)),
                             md5 = c(md5(XWALK_FILE), md5(PANEL_FILE)))
cat("\nM0 complete. Next: M1_counties.R\n")
