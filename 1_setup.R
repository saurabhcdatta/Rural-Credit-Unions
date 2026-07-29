###############################################################################
# 1_setup.R  -- paths, packages, helpers
#
# HOW TO USE THESE SCRIPTS
#   Run them in order, 1 through 8. Select a block and hit Ctrl+Enter; every
#   object stays in the environment so you can inspect it. Lines marked
#   ## LOOK are there to be run and read, not just executed.
#
#   Each script assumes the previous ones have run in the same session.
###############################################################################

library(data.table)

## ---- 1.1 paths --------------------------------------------------------------
CODE_DIR   <- "C:/Users/sdatta/OneDrive - NCUA/Rural_CUs/code"
DATA_DIR   <- "C:/Users/sdatta/OneDrive - NCUA/Rural_CUs/data"
CACHE_DIR  <- file.path(DATA_DIR, "raw")
BRANCH_DIR <- file.path(DATA_DIR, "branch_files")
OUT_DIR    <- "C:/Users/sdatta/OneDrive - NCUA/Rural_CUs/output"

XWALK_FILE <- file.path(DATA_DIR, "fips_national_final_withpostCensus2000updates.dta")
PANEL_FILE <- list.files(DATA_DIR, pattern = "^OCE_CallReport_.*\\.dta$", full.names = TRUE)[1]

for (d in c(CACHE_DIR, BRANCH_DIR, OUT_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
setwd(CODE_DIR)

## LOOK -- do the inputs exist?
file.exists(XWALK_FILE)
PANEL_FILE
file.exists(PANEL_FILE)

## ---- 1.2 packages -----------------------------------------------------------
need <- c("data.table", "haven", "fixest")
missing <- need[!sapply(need, requireNamespace, quietly = TRUE)]
missing                                                   ## LOOK -- should be empty
if (length(missing)) install.packages(missing)

if (.Platform$OS.type == "windows") options(download.file.method = "wininet")

## ---- 1.3 rural code sets ----------------------------------------------------
## 12 CFR 1026.35(b)(2)(iv)(A)(1): a county in neither an MSA nor a micropolitan
## area adjacent to an MSA. In UIC terms that means: not metro, and not
## (micropolitan AND metro-adjacent).
##
## 2024 UIC (9 categories): 1 large metro, 2 micro adj large metro,
##   3 noncore adj large metro, 4 small metro, 5 micro adj small metro,
##   6 noncore adj small metro, 7 micro NOT adj metro, 8/9 noncore nonadjacent
RURAL_2024 <- c(3, 6, 7, 8, 9)

## 2013 UIC (12 categories): 1-2 metro, 3/5 micro adj metro, 8 micro nonadj,
##   4/6/7/9/10/11/12 noncore
RURAL_2013 <- c(4, 6, 7, 8, 9, 10, 11, 12)

## ---- 1.4 county-name helpers ------------------------------------------------
## Needed as functions because they are vectorised over thousands of names.
## The "|city" suffix keeps Virginia's Franklin County apart from Franklin city.

norm_nm <- function(x) {
  s <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  s <- tolower(s)
  s <- gsub("[-.'`,]", " ", s)
  gsub("\\s+", " ", trimws(s))
}

city_key <- function(x) {
  s    <- norm_nm(x)
  city <- grepl("\\bcity\\b\\s*$", s) & !grepl("city and borough", s)
  b    <- gsub("\\b(county|parish|borough|census area|municipality|municipio|planning region|city and borough)\\b", " ", s)
  b    <- gsub("\\bcity\\b\\s*$", "", b)
  b    <- gsub("\\bst\\b", "saint", b)
  b    <- gsub("\\bste\\b", "sainte", b)
  paste0(gsub("\\s+", " ", trimws(b)), ifelse(city, "|city", "|county"))
}

## LOOK -- sanity check the keys
city_key(c("Autauga County", "Franklin city", "Franklin County",
           "Prince of Wales-Hyder Census Area", "James City County", "St. Louis"))

## ---- 1.5 FIPS that changed identity -----------------------------------------
## A 2012 branch file says "Shannon" and "Wade Hampton"; no current reference
## has those. Map historical codes forward BEFORE joining the rural table.
FIPS_FIX <- data.table(
  old  = c("02261","02270","46113","51515","12025","30113","51780"),
  new  = c("02063","02158","46102","51019","12086","30067","51083"),
  note = c("Valdez-Cordova AK dissolved 2019 -> Chugach (Copper River is the other successor; both non-rural)",
           "Wade Hampton AK renamed 2015 -> Kusilvak",
           "Shannon SD renamed 2015 -> Oglala Lakota",
           "Bedford city VA reverted to town 2013 -> Bedford County",
           "Dade FL renamed 1997 -> Miami-Dade",
           "Yellowstone Nat'l Park County MT abolished 1997 -> Park (JUDGMENT: Gallatin is metro, Park is rural; settled MT portion is in Park)",
           "South Boston city VA reverted to town 1995 -> Halifax County"))
FIPS_FIX                                                  ## LOOK

TERRITORIES <- c("GU","VI","AS","MP","FM","MH","PW")

cat("\nSetup complete. Next: 2_rural_counties.R\n")
