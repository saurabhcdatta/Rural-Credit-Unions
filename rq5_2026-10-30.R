###############################################################################
# rq5.R  --  RESEARCH QUESTION 5
# "Does the definition of 'rural' change the answer?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# THE QUESTION IN PLAIN WORDS
#   The statute points to one definition of rural (12 CFR 1026.35: the credit
#   union's headquarters county, 2024 USDA-ERS Urban Influence Codes). A
#   reader can fairly ask: would the RQ1 findings survive a different, also
#   reasonable, definition? If they would not, every number in the report is
#   an artefact of the definition and has to be presented that way.
#
# WHAT IS COMPARED  (each one is a column or a row filter on cr)
#   D0  baseline       HQ county rural, 2024 codes                    (the study)
#   D1  vintage 2013   HQ county rural, 2013 codes                    (older map)
#   D2  remote only    HQ county rural AND not next to any metro      (stricter)
#   D3  nonmetro       HQ county outside any metro area (UIC not 1 or 4) (wider; USDA's usual)
#   D4  offices S2     most of the credit union's offices are rural   (service area)
#   D5  offices any    at least one office is rural                   (widest service area)
#   D6  lower 48       baseline, dropping Alaska, Hawaii, territories (sample check)
#
# THE TEST, WRITTEN DOWN BEFORE RUNNING (handoff v4, Q5)
#   For each definition, re-run the RQ1 headline model (state x quarter fixed
#   effects, size and charter controls, clustered by CU and county) and compare
#   the rural coefficient with the baseline coefficient ON THE SAME ROWS.
#   PASS = same sign AND within +/-20% of the baseline.
#   Expectation: every definition passes on growth and ROA; capital may not,
#   because the capital advantage was already small and not robust in RQ1.
#
# REQUIRES  cr, county, sites in memory (rq_load.R or the legacy shim), and
#           universe.R in CODE_DIR. Packages: dplyr, tidyr, fixest.
#
# Sequential blocks. Run each, read what it prints (## LOOK), then move on.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)   # cr and sites are data.tables; loaded first so dplyr's verbs take precedence
  library(dplyr)
  library(tidyr)
  library(fixest)
})

## ---- 5.0  gates -----------------------------------------------------------------
if (!exists("assert_universe")) source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "universe.R"))
assert_universe(cr, "rq5.R")
stopifnot(exists("county"), exists("sites"))

OUT5 <- file.path(OUT_DIR, "rq5")
dir.create(OUT5, recursive = TRUE, showWarnings = FALSE)
q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
QZ <- max(cr$qidx)
cat("Latest quarter:", q_lab(QZ), "\n")
## the 2024 rural codes come from setup (one definition, one place); stop if missing
stopifnot("RURAL_2024 not defined -- run the setup script first" = exists("RURAL_2024"))
if (!exists("METRO_2024")) METRO_2024 <- c(1L, 4L)   # ERS 2024 UIC: 1 large metro, 4 small metro (code 2 is micropolitan)

## the same controls as RQ1, so the baseline row reproduces the RQ1 number
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"
OUTCOMES <- c(g_assets = "Asset growth", roa_pct = "Return on assets", networth_pct = "Net worth ratio",
              g_members = "Membership growth", g_loans = "Loan growth")
need <- c("cu_number", "qidx", "fips", "state_code", "rural", "est_sample",
          "ln_assets", "age_q", "lid", "mdi", "fed", "cecl_flag", names(OUTCOMES))
missing <- setdiff(need, names(cr))
if (length(missing)) stop("cr is missing: ", paste(missing, collapse = ", "))

## ---- 5.1  build every definition on the credit union x quarter panel ---------------
## County facts for the headquarters county
cty <- county %>%
  as_tibble() %>%
  select(fips, state, uic24, uic13, rural13, adj_class)

d <- cr %>%
  as_tibble() %>%
  select(all_of(need)) %>%
  left_join(cty, by = "fips")

## D1 2013 vintage. Connecticut changed its county codes in 2022; no Connecticut
## county is rural under either vintage (checked in 2_rural_counties.R), so a
## missing 2013 code there is a 0, not a gap.
d <- d %>%
  mutate(rural13 = ifelse(is.na(rural13) & state %in% "CT", 0L, rural13))

## D2 remote only and D3 nonmetro, both from the 2024 codes
d <- d %>%
  mutate(def_2024_hq   = rural,
         def_2013_hq   = rural13,
         def_remote    = as.integer(rural == 1L & adj_class %in% c("remote micropolitan", "remote")),
         def_nonmetro  = ifelse(is.na(uic24), NA_integer_, as.integer(!uic24 %in% METRO_2024)))

## D4 and D5 from the office file: share of each credit union's offices that sit
## in rural counties (2024 codes), every quarter. Offices start in 2010Q3.
## (the master's sites carries foreign / terr flags; older builds may not -- treat missing as FALSE)
sites_t <- as_tibble(sites)
for (v in c("foreign", "terr")) if (!v %in% names(sites_t)) sites_t[[v]] <- FALSE
if (!"in_cr" %in% names(sites_t)) sites_t$in_cr <- sites_t$cu_number %in% unique(cr$cu_number)
office_share <- sites_t %>%
  filter(in_cr == TRUE, foreign == FALSE, terr == FALSE, !is.na(fips)) %>%
  left_join(cty %>% transmute(fips, rural24_office = as.integer(uic24 %in% RURAL_2024)), by = "fips") %>%
  group_by(cu_number, qidx) %>%
  summarise(n_offices = n(),
            share_rural_offices = mean(rural24_office, na.rm = TRUE),
            .groups = "drop")

d <- d %>%
  left_join(office_share, by = c("cu_number", "qidx")) %>%
  mutate(def_offices_s2  = ifelse(is.na(share_rural_offices), NA_integer_, as.integer(share_rural_offices >= 0.5)),
         def_offices_any = ifelse(is.na(share_rural_offices), NA_integer_, as.integer(share_rural_offices > 0)),
         lower48 = !(state_code %in% c("AK", "HI")) & as.integer(substr(fips, 1, 2)) <= 56L)

## quick sanity: the office definitions must exist for most rows after 2010Q3
d %>%
  filter(qidx == QZ) %>%
  summarise(credit_unions = n(),
            pct_with_offices = round(100 * mean(!is.na(share_rural_offices)), 1),
            pct_with_2013_code = round(100 * mean(!is.na(def_2013_hq)), 1)) %>%
  print()                                                                        ## LOOK -- both should be near 100

## ---- 5.2  how many credit unions are rural under each definition? --------------------
DEFS <- c(def_2024_hq     = "D0 Baseline: HQ county, 2024 codes",
          def_2013_hq     = "D1 HQ county, 2013 codes",
          def_remote      = "D2 Remote only (not next to a metro)",
          def_nonmetro    = "D3 Nonmetro (any county outside a metro)",
          def_offices_s2  = "D4 Most offices rural",
          def_offices_any = "D5 Any office rural")

latest <- d %>% filter(qidx == QZ)
counts <- bind_rows(lapply(names(DEFS), function(v) {
  x <- latest[[v]]; b <- latest$def_2024_hq
  tibble(definition = DEFS[[v]],
         rural_cus  = sum(x == 1L, na.rm = TRUE),
         in_both    = sum(x == 1L & b == 1L, na.rm = TRUE),
         baseline_only = sum(x == 0L & b == 1L, na.rm = TRUE),
         this_only  = sum(x == 1L & b == 0L, na.rm = TRUE),
         pct_agree_all_cus = round(100 * mean(x == b, na.rm = TRUE), 2))
}))
print(counts, width = Inf)                                                        ## LOOK -- D0 should say 463
write.csv(counts, file.path(OUT5, "rq5_counts.csv"), row.names = FALSE)

## ---- 5.3  the rural coefficient under each definition --------------------------------
## Estimation sample, exactly as RQ1: assets >= $1m.
est <- d %>%
  filter(est_sample == TRUE, !is.na(def_2024_hq)) %>%
  mutate(st = factor(state_code), qtr = factor(qidx))

## One helper: the coefficient on a 0/1 rural variable, with the RQ1 model.
rural_beta <- function(y, rural_var, data) {
  data$rural_x <- data[[rural_var]]
  m <- feols(as.formula(sprintf("%s ~ rural_x + %s | st^qtr", y, CTRL)),
             data = data, cluster = ~ cu_number + fips)
  ct <- coeftable(m)["rural_x", ]
  c(beta = unname(ct["Estimate"]), se = unname(ct["Std. Error"]), p = unname(ct["Pr(>|t|)"]), n = nobs(m))
}

## Each row of SPECS: which rural variable, and which rows it is run on.
## The baseline is re-estimated on the SAME rows, so a difference can only come
## from the definition, never from a different sample (offices start in 2010Q3).
SPECS <- tribble(
  ~spec,                                            ~rural_var,        ~rows,
  "D1 HQ county, 2013 codes",                       "def_2013_hq",     "has_2013",
  "D2 Remote only (not next to a metro)",           "def_remote",      "all",
  "D3 Nonmetro (any county outside a metro)",       "def_nonmetro",    "has_uic",
  "D4 Most offices rural",                          "def_offices_s2",  "has_offices",
  "D5 Any office rural",                            "def_offices_any", "has_offices",
  "D6 Lower 48 only (drop AK, HI, territories)",    "def_2024_hq",     "lower48"
)
row_filter <- function(rows) switch(rows,
  all         = est,
  has_2013    = filter(est, !is.na(def_2013_hq)),
  has_uic     = filter(est, !is.na(def_nonmetro)),
  has_offices = filter(est, !is.na(def_offices_s2)),
  lower48     = filter(est, lower48))

res <- list()
for (i in seq_len(nrow(SPECS))) {
  rows_i <- row_filter(SPECS$rows[i])
  for (y in names(OUTCOMES)) {
    alt  <- rural_beta(y, SPECS$rural_var[i], rows_i)
    ## baseline comparator: same rows, except D6 whose point IS the different rows
    base <- if (SPECS$rows[i] == "lower48") rural_beta(y, "def_2024_hq", est) else rural_beta(y, "def_2024_hq", rows_i)
    res[[length(res) + 1]] <- tibble(spec = SPECS$spec[i], outcome = OUTCOMES[[y]],
                                     baseline_beta = base["beta"], alt_beta = alt["beta"],
                                     alt_p = alt["p"], n_rows = alt["n"])
  }
  cat("done:", SPECS$spec[i], "\n")
}
R5 <- bind_rows(res) %>%
  mutate(ratio     = alt_beta / baseline_beta,
         same_sign = sign(alt_beta) == sign(baseline_beta),
         within_20 = abs(ratio - 1) <= 0.20,
         verdict   = case_when(same_sign & within_20 ~ "PASS",
                               same_sign             ~ "same direction, size differs",
                               TRUE                  ~ "FLIPS"),
         across(c(baseline_beta, alt_beta, ratio), ~ round(.x, 3)),
         alt_p = signif(alt_p, 2))
print(R5, n = Inf, width = Inf)                                                  ## LOOK -- the whole answer is in this table

## the baseline itself, full sample -- must reproduce RQ1 (growth ~ +0.99, ROA ~ +0.17, net worth ~ +0.41)
base_full <- bind_rows(lapply(names(OUTCOMES), function(y)
  tibble(outcome = OUTCOMES[[y]], beta = round(rural_beta(y, "def_2024_hq", est)["beta"], 3))))
print(base_full)                                                                 ## LOOK -- if these differ from RQ1, stop: something upstream changed

## ---- 5.4  the verdict, one line per definition ------------------------------------
verdict <- R5 %>%
  group_by(spec) %>%
  summarise(outcomes_pass = sum(verdict == "PASS"),
            outcomes_same_direction = sum(same_sign),
            outcomes_flip = sum(!same_sign),
            growth = verdict[outcome == "Asset growth"],
            roa    = verdict[outcome == "Return on assets"],
            capital = verdict[outcome == "Net worth ratio"],
            .groups = "drop")
print(verdict, width = Inf)                                                      ## LOOK -- the slide

## ---- 5.5  who changes category, and why ------------------------------------------
## The credit unions the baseline calls rural but the office rule does not (and
## the reverse) -- the cases a reader will ask about.
switchers <- latest %>%
  filter(!is.na(def_offices_s2), def_2024_hq != def_offices_s2) %>%
  transmute(cu_number, state_code, fips,
            direction = ifelse(def_2024_hq == 1L, "rural HQ, mostly non-rural offices", "non-rural HQ, mostly rural offices"),
            n_offices, share_rural_offices = round(share_rural_offices, 2))
switchers %>% count(direction) %>% print()                                       ## LOOK
write.csv(switchers, file.path(OUT5, "rq5_office_rule_switchers.csv"), row.names = FALSE)

## ---- 5.6  save, register, print the slide numbers -------------------------------
write.csv(R5, file.path(OUT5, "rq5_coefficients_by_definition.csv"), row.names = FALSE)
write.csv(verdict, file.path(OUT5, "rq5_verdict.csv"), row.names = FALSE)
if (exists("register_result")) {
  for (i in seq_len(nrow(verdict)))
    register_result(5, paste0("pass_", gsub("[^a-z0-9]+", "_", tolower(verdict$spec[i]))),
                    sprintf("%d of %d pass; %d flip", verdict$outcomes_pass[i], length(OUTCOMES), verdict$outcomes_flip[i]), "")
}

cat("\n=== SLIDE NUMBERS, Q5 ===\n")
cat(sprintf("Rural credit unions, %s: %s\n", q_lab(QZ),
            paste(sprintf("%s %d", sub(":.*", "", counts$definition), counts$rural_cus), collapse = " | ")))
cat(sprintf("Growth advantage: baseline %+.2f; range across definitions %+.2f to %+.2f; %d of %d definitions pass.\n",
            base_full$beta[1], min(R5$alt_beta[R5$outcome == "Asset growth"]), max(R5$alt_beta[R5$outcome == "Asset growth"]),
            sum(R5$verdict[R5$outcome == "Asset growth"] == "PASS"), nrow(SPECS)))
cat(sprintf("ROA advantage: baseline %+.2f; range %+.2f to %+.2f; %d of %d pass.\n",
            base_full$beta[2], min(R5$alt_beta[R5$outcome == "Return on assets"]), max(R5$alt_beta[R5$outcome == "Return on assets"]),
            sum(R5$verdict[R5$outcome == "Return on assets"] == "PASS"), nrow(SPECS)))
cat(sprintf("Any sign flips on growth or ROA: %s\n",
            ifelse(any(!R5$same_sign[R5$outcome %in% c("Asset growth", "Return on assets")]), "YES -- read 5.3 before writing anything", "none")))
