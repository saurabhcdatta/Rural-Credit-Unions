## =============================================================================
## 15_state_table.R
## State-level rural credit union table for the executive briefing
##
## Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
##
## Produces one table with EVERY column filled for the states that matter, so
## the briefing slide no longer has to show two separate rankings with holes in
## them. The union of the top states on each measure is what the slide wants.
##
## Requires in the environment: cr (the 5300 panel with the rural flag attached,
## from 3_callreport.R). Run 1_ -> 2_ -> 3_ first.
##
## Blocks:
##   15.1  column resolution and the current-quarter cut
##   15.2  the state table
##   15.3  the rankings, and the union table the slide uses
##   15.4  paste-ready output
## =============================================================================

suppressPackageStartupMessages({ library(data.table) })

OUT_DIR <- if (exists("OUT_DIR")) OUT_DIR else "output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

TOP_N         <- 6L          # how many states to take from each ranking
CU_TYPES_KEEP <- c(1L, 2L)   # federal and federally insured state charters

## ---- 15.1  columns and the current quarter ---------------------------------
## Resolve by pattern. Column names have drifted across builds on this project
## and a hardcoded name fails silently on whichever build you did not test.

stopifnot(exists("cr"))
if (!is.data.table(cr)) setDT(cr)

pick_col <- function(DT, patterns, what, required = TRUE) {
  nm <- names(DT)
  for (p in patterns) {
    hit <- grep(p, nm, ignore.case = TRUE, value = TRUE)
    if (length(hit)) {
      hit <- hit[order(nchar(hit))]
      if (length(hit) > 1L)
        message(sprintf("  [%s] %d candidates (%s) -> '%s'",
                        what, length(hit), paste(hit, collapse = ", "), hit[1]))
      return(hit[1])
    }
  }
  if (required)
    stop(sprintf("Cannot resolve '%s'. Columns present: %s", what,
                 paste(nm, collapse = ", ")))
  NA_character_
}

C <- list(
  st      = pick_col(cr, c("^state_code$", "^state$", "^statecode$"), "state"),
  year    = pick_col(cr, c("^year$", "^yr$"),                          "year"),
  qtr     = pick_col(cr, c("^quarter$", "^qtr$", "^q$"),               "quarter"),
  assets  = pick_col(cr, c("^assets_tot$", "^assets_total$", "^assets"), "assets"),
  members = pick_col(cr, c("^members$"),                               "members"),
  rural   = pick_col(cr, c("rural.*24", "rural.*2024", "^rural$"),     "rural flag"),
  cutype  = pick_col(cr, c("^cu_type$", "^cutype$"),                   "cu type", FALSE)
)
cat("Resolved:", paste(names(C), unlist(C), sep = " = ", collapse = " | "), "\n")

cr[, .qidx := as.integer(get(C$year)) * 4L + as.integer(get(C$qtr))]
QMAX <- max(cr$.qidx, na.rm = TRUE)
cur  <- cr[.qidx == QMAX]
if (!is.na(C$cutype)) cur <- cur[get(C$cutype) %in% CU_TYPES_KEEP]
cat(sprintf("Current quarter: %dQ%d | %s credit unions\n",
            (QMAX - 1L) %/% 4L, ((QMAX - 1L) %% 4L) + 1L,
            format(nrow(cur), big.mark = ",")))

## ---- 15.2  the state table -------------------------------------------------
## State identifier can be a FIPS number or a postal code depending on the
## build, so handle both and fail loudly if it is neither.

FIPS_ST <- c(
  "01"="Alabama","02"="Alaska","04"="Arizona","05"="Arkansas","06"="California",
  "08"="Colorado","09"="Connecticut","10"="Delaware","11"="District of Columbia",
  "12"="Florida","13"="Georgia","15"="Hawaii","16"="Idaho","17"="Illinois",
  "18"="Indiana","19"="Iowa","20"="Kansas","21"="Kentucky","22"="Louisiana",
  "23"="Maine","24"="Maryland","25"="Massachusetts","26"="Michigan","27"="Minnesota",
  "28"="Mississippi","29"="Missouri","30"="Montana","31"="Nebraska","32"="Nevada",
  "33"="New Hampshire","34"="New Jersey","35"="New Mexico","36"="New York",
  "37"="North Carolina","38"="North Dakota","39"="Ohio","40"="Oklahoma","41"="Oregon",
  "42"="Pennsylvania","44"="Rhode Island","45"="South Carolina","46"="South Dakota",
  "47"="Tennessee","48"="Texas","49"="Utah","50"="Vermont","51"="Virginia",
  "53"="Washington","54"="West Virginia","55"="Wisconsin","56"="Wyoming",
  "60"="American Samoa","66"="Guam","69"="Northern Mariana Islands",
  "72"="Puerto Rico","78"="US Virgin Islands")

raw_st <- as.character(cur[[C$st]])
if (all(grepl("^[0-9]+$", raw_st[!is.na(raw_st)]))) {
  cur[, state_name := FIPS_ST[formatC(as.integer(get(C$st)), width = 2, flag = "0")]]
} else if (all(nchar(trimws(raw_st[!is.na(raw_st)])) == 2)) {
  pc <- toupper(trimws(raw_st))
  cur[, state_name := c(setNames(state.name, state.abb),
                        "DC" = "District of Columbia",
                        "PR" = "Puerto Rico", "GU" = "Guam",
                        "VI" = "US Virgin Islands")[pc]]
} else {
  stop("Could not interpret the state column as FIPS or postal code.")
}
if (anyNA(cur$state_name))
  warning(sum(is.na(cur$state_name)), " credit unions have an unrecognised state code")

ST <- cur[!is.na(state_name), list(
  cus          = .N,
  rural_cus    = sum(get(C$rural) == 1L, na.rm = TRUE),
  rural_assets = sum(as.numeric(get(C$assets))[get(C$rural) == 1L], na.rm = TRUE),
  rural_members= sum(as.numeric(get(C$members))[get(C$rural) == 1L], na.rm = TRUE)
), by = state_name]
ST[, rural_share := 100 * rural_cus / cus]
ST[, rural_assets_bn := rural_assets / 1e9]
setorder(ST, -rural_cus)

cat(sprintf("\n%d states/territories with credit unions; %d have at least one rural CU\n",
            nrow(ST), ST[rural_cus > 0, .N]))
fwrite(ST, file.path(OUT_DIR, "state_rural_table.csv"))

## ---- 15.3  rankings, and the union table the slide uses --------------------
## A state qualifies for the slide if it is top-N on EITHER measure. Reporting
## the union with every column filled is more honest than two ranked lists,
## because it shows the states that lead on one measure and not the other.
##
## MIN_CUS guards the share ranking: a state with three credit unions, two of
## them rural, is not a meaningful "67% rural" and would embarrass us on a slide.

MIN_CUS <- 10L

top_share  <- ST[cus >= MIN_CUS][order(-rural_share)][1:TOP_N, state_name]
top_assets <- ST[order(-rural_assets)][1:TOP_N, state_name]
top_count  <- ST[order(-rural_cus)][1:TOP_N, state_name]

cat("\n--- Top", TOP_N, "by rural share of the state's credit unions (min",
    MIN_CUS, "CUs) ---\n")
print(ST[state_name %in% top_share][order(-rural_share),
        list(state_name, cus, rural_cus, rural_share = round(rural_share, 1),
             rural_assets_bn = round(rural_assets_bn, 2))])

cat("\n--- Top", TOP_N, "by total rural credit union assets ---\n")
print(ST[state_name %in% top_assets][order(-rural_assets),
        list(state_name, cus, rural_cus, rural_share = round(rural_share, 1),
             rural_assets_bn = round(rural_assets_bn, 2))])

cat("\n--- Top", TOP_N, "by number of rural credit unions ---\n")
print(ST[state_name %in% top_count][order(-rural_cus),
        list(state_name, cus, rural_cus, rural_share = round(rural_share, 1),
             rural_assets_bn = round(rural_assets_bn, 2))])

SLIDE <- ST[state_name %in% unique(c(top_share, top_assets, top_count))][
  order(-rural_assets),
  list(state_name,
       rural_cus,
       rural_share_pct  = round(rural_share, 1),
       rural_assets_bn  = round(rural_assets_bn, 2),
       rural_members_k  = round(rural_members / 1e3, 1))]

cat("\n=================== TABLE FOR THE SLIDE ===================\n")
print(SLIDE)
cat("==========================================================\n")
fwrite(SLIDE, file.path(OUT_DIR, "state_rural_slide_table.csv"))

## ---- 15.4  paste-ready output ----------------------------------------------
## Prints the rows already formatted, so the slide can be updated by copying
## rather than by retyping numbers - which is where transcription errors enter.

cat("\n--- Paste-ready rows (State | Rural CUs | Rural share | Rural assets) ---\n")
for (i in seq_len(nrow(SLIDE))) {
  cat(sprintf('["%s", "%d", "%.0f%%", "$%.1fbn"],\n',
              SLIDE$state_name[i], SLIDE$rural_cus[i],
              SLIDE$rural_share_pct[i], SLIDE$rural_assets_bn[i]))
}

cat("\nSanity check against the exhibit pack: national rural CU count",
    ST[, sum(rural_cus)], "and rural assets $",
    round(ST[, sum(rural_assets)] / 1e9, 1), "bn.\n")
cat("If those do not match the 468 credit unions and $63.1bn already reported,\n",
    "the cu_type filter or the rural flag differs from the exhibit-pack run and\n",
    "the discrepancy must be resolved before either figure goes on a slide.\n", sep = "")
