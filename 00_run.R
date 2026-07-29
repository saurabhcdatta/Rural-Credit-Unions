###############################################################################
# 00_run.R  --  driver for the ROAD Act Sec. 909 Rural Credit Unions Study
#
# Run this ONE file. It checks the environment, then executes in order.
# Nothing else needs to be sourced by hand.
#
# Production notes (NCUA managed Windows workstation):
#   * Keep DATA OUT OF ONEDRIVE. The UIC files and the stacked panel cache are
#     large and churn on every write; OneDrive will sync-lock them mid-write
#     and R will fail with permission errors that look like corruption.
#     Code in OneDrive is fine. Data goes on a local or network drive.
#   * Downloads usually fail on the first try behind the agency proxy. The
#     wininet fallback is built in; if all methods fail the console prints
#     manual instructions with the exact target path.
###############################################################################

## ---------------------------------------------------------------------------
## 1. PATHS  -- edit these four, nothing else
## ---------------------------------------------------------------------------

CODE_DIR  <- "C:/Users/sdatta/OneDrive - NCUA/Rural_CUs/code"
DATA_DIR  <- "C:/Users/sdatta/Rural_CUs_data"      # NOT in OneDrive
PANEL_DIR <- "C:/Users/sdatta/Rural_CUs_data/dta"  # OCE_CallReport_*.dta live here
OUT_DIR   <- "C:/Users/sdatta/Rural_CUs_data/out"

CACHE_DIR <- file.path(DATA_DIR, "raw")

setwd(CODE_DIR)
for (d in c(DATA_DIR, PANEL_DIR, CACHE_DIR, OUT_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

cat("working directory :", getwd(), "\n")
cat("data directory    :", DATA_DIR, "\n")

if (grepl("OneDrive", DATA_DIR, ignore.case = TRUE))
  warning("DATA_DIR is inside OneDrive. Sync will lock files mid-write. ",
          "Move it to a local or network drive.", call. = FALSE)

## ---------------------------------------------------------------------------
## 2. PACKAGE CHECK  -- fail loudly and early, not halfway through
## ---------------------------------------------------------------------------
## install.packages() may be restricted on a managed workstation. This reports
## everything missing at once so a single request covers it.

need <- c(
  data.table = "core",  haven    = "read Stata .dta",
  fixest     = "Q1 regressions",
  readxl     = "read the 2013 UIC .xls",
  digest     = "checksum the data manifest",
  openxlsx   = "formatted exhibit workbook",
  ggplot2    = "maps",  sf       = "maps",  tigris = "county geometry"
)
missing <- names(need)[!vapply(names(need), requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  cat("\nMISSING PACKAGES:\n")
  for (m in missing) cat("  ", m, " -- ", need[[m]], "\n", sep = "")
  cat("\nTry:  install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))\n", sep = "")
  cat("If blocked by policy, send this list to IT as one request.\n")
  cat("data.table + haven + fixest are enough for Q1; the rest can wait.\n\n")
} else cat("all packages present\n")

## Behind TLS inspection, libcurl rejects the agency root CA. wininet uses the
## Windows certificate store, which has it.
if (.Platform$OS.type == "windows") options(download.file.method = "wininet")

## ---------------------------------------------------------------------------
## 3. RURAL CLASSIFICATION
## ---------------------------------------------------------------------------

source(file.path(CODE_DIR, "rural_definition.R"))

uic_status(cache_dir = CACHE_DIR)          # what is cached, and where
uic <- uic_setup(cache_dir = CACHE_DIR)    # <-- THIS is what creates `uic`

ct_matters(uic$u2013, uic$u2024)                    # does Connecticut matter?
shift <- rural_vintage_shift(uic$u2013, uic$u2024)  # the 169 reclassified counties

## STOP HERE on the first run and confirm validate_uic() showed all diffs = 0
## and 1,556 rural counties. Everything downstream depends on that join.

## ---------------------------------------------------------------------------
## 4. PANEL + EXHIBITS + Q1
## ---------------------------------------------------------------------------
## Comment these back in once section 3 is verified. The first prep_panel()
## call reads every .dta and caches the stack -- slow once, fast thereafter.

# source(file.path(CODE_DIR, "panel_prep.R"))
#
# ## Confirm the Stata coding of cu_type before it becomes a regression control
# stata_value_labels(list.files(PANEL_DIR, "\\.dta$", full.names = TRUE)[1])
#
# P  <- prep_panel(PANEL_DIR, cache_dir = CACHE_DIR)
# cr <- P$cr
#
# source(file.path(CODE_DIR, "exhibits_descriptive.R"))   # tables first
# source(file.path(CODE_DIR, "rural_maps.R"))
# source(file.path(CODE_DIR, "q1_rural_vs_small.R"))      # econometrics last

cat("\nSection 3 complete. Verify the validation output, then uncomment section 4.\n")
