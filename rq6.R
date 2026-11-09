###############################################################################
# rq6.R  --  RESEARCH QUESTION 6
# "Does the rural loan book differ meaningfully?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# THE QUESTION IN PLAIN WORDS
#   RQ1-5 asked how fast rural credit unions grow and how much they earn. RQ6
#   asks WHAT they lend: is the mix of loans different from similar credit
#   unions elsewhere, and is farm lending a real part of it?
#
# HYPOTHESIS AND EXPECTATION (handoff v4, written before running)
#   Like-for-like, rural loan books carry more real-estate and agricultural
#   credit and less consumer credit. Expect: more first-lien real estate and
#   auto, less credit card; business lending absent at about two thirds of
#   rural credit unions.
#
# WHAT EACH BLOCK DOES
#   6.0  gates, and read the loan columns 3_callreport.R did not keep
#   6.0b farm loans from the AgLending build (codes 042A5-A9 etc., 2017Q3 on)
#   6.1  map panel columns to eight loan types  (CHECK THE MAPPING BEFORE GOING ON)
#   6.2  shares of the loan book, rural vs other, raw
#   6.3  like-for-like: each share on rural, same model as RQ1
#   6.4  does it hold in every size band?
#   6.5  farm lending: who does it, how much, and in farming counties
#   6.6  has the gap changed over time?
#   6.7  credit quality by loan type (where the panel has it)
#   6.8  chart, CSVs, slide numbers
#
# REQUIRES  cr (3_callreport.R or rq_load.R), county, PANEL_FILE, OUT_DIR,
#           CODE_DIR. Packages: data.table, dplyr, tidyr, fixest, ggplot2, haven.
#
# Sequential blocks. Run each, read what it prints (## LOOK), then move on.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(fixest); library(ggplot2)
})

## ---- 6.0  gates, and the extra loan columns ---------------------------------------
if (!exists("assert_universe")) source(file.path(CODE_DIR, "universe.R"))
assert_universe(cr, "rq6.R")
stopifnot("PANEL_FILE not set -- run the setup script" = exists("PANEL_FILE"))
OUT6 <- file.path(OUT_DIR, "rq6"); dir.create(OUT6, recursive = TRUE, showWarnings = FALSE)
if (!exists("q_lab")) q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
QZ <- max(cr$qidx)
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"     # as RQ1

## Every loan, delinquency and agricultural column in the Stata panel.
hdr <- haven::read_dta(PANEL_FILE, n_max = 0)
loan_cols <- grep("^lns_|^ln_|loan|agri|^ag_|farm|^dq_", names(hdr), ignore.case = TRUE, value = TRUE)
loan_cols <- setdiff(loan_cols, "ln_assets")
print(loan_cols)                                                                ## LOOK -- the loan fields available

## A short description from the Stata variable labels, if the panel carries them
lbl <- sapply(loan_cols, function(v) { l <- attr(hdr[[v]], "label"); if (is.null(l)) "" else l })
print(data.frame(column = loan_cols, label = unname(lbl)), right = FALSE)       ## LOOK -- what each one is

## Read just those columns (plus keys) -- quick, the file is read column-wise.
extra <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(c("cu_number", "year", "quarter", loan_cols)))
extra <- haven::zap_labels(extra) %>%
  mutate(cu_number = as.numeric(cu_number),
         qidx = as.integer(year) * 4L + as.integer(quarter)) %>%
  select(-year, -quarter)
## keep only columns cr does not already have
extra <- extra[, c("cu_number", "qidx", setdiff(names(extra), c(names(cr), "cu_number", "qidx")))]

d <- cr %>% as_tibble() %>% left_join(extra, by = c("cu_number", "qidx"))
cat("Panel rows:", format(nrow(d), big.mark = ","), "| extra loan columns added:", ncol(extra) - 2, "\n")

## ---- 6.0b  farm loans: the AgLending build --------------------------------------------
## Farm loans are NOT in the OCE combined panel. They come from the 5300 report
## database (FS220L/M/P), pulled by the AgLending Stata job, which saves them as
## all_in.dta keyed by join_number and Stata quarter (q_period). Codes:
##   042A5 member, secured by farmland        042A6 member, production & other farm
##   042A7 nonmember participations, farmland 042A8 nonmember participations, production
##   042A9 total agricultural commercial loans (= A5+A6+A7+A8)
##   463A5-A8 YTD dollars granted/purchased (same four splits)
##   DL0090 / DL0118 reportable delinquency (60+ days), farmland / production
##   CH0025 / CH0033 YTD charge-offs, farmland / production
## Balances from 2017Q3; delinquency and charge-offs from 2022Q1. Farm loans are
## COMMERCIAL loans: they sit inside business lending, never added to it.
AG_FILE <- "S:/Projects/AgLending/2026Q2/all_in.dta"     # point this at the AgLending output
AG_VARS <- c("acct_042a5", "acct_042a6", "acct_042a7", "acct_042a8", "acct_042a9",
             "acct_463a5", "acct_463a6", "acct_463a7", "acct_463a8",
             "acct_dl0090", "acct_dl0118", "acct_ch0025", "acct_ch0033")
stopifnot("AG_FILE not found -- ask for all_in.dta from the AgLending folder" = file.exists(AG_FILE))
agraw <- haven::read_dta(AG_FILE, col_select = tidyselect::any_of(c("join_number", "q_period", AG_VARS)))
print(setdiff(AG_VARS, names(agraw)))                                            ## LOOK -- should be empty
ag <- haven::zap_labels(agraw) %>%
  mutate(join_number = as.numeric(join_number),
         ## Stata %tq counts quarters from 1960q1 = 0; our qidx = year * 4 + quarter
         qidx = (1960L + as.integer(q_period) %/% 4L) * 4L + (as.integer(q_period) %% 4L + 1L)) %>%
  transmute(join_number, qidx,
            ag_total      = acct_042a9,
            ag_farmland   = acct_042a5 + acct_042a7,
            ag_production = acct_042a6 + acct_042a8,
            ag_nonmember  = acct_042a7 + acct_042a8,
            ag_ytd        = acct_463a5 + acct_463a6 + acct_463a7 + acct_463a8,
            ag_dq60       = acct_dl0090 + acct_dl0118,
            ag_chgoff_ytd = acct_ch0025 + acct_ch0033) %>%
  distinct(join_number, qidx, .keep_all = TRUE)
## check: the four parts add up to the total (the AgLending job asserts this too)
ag %>% summarise(rows = n(), max_gap = max(abs(ag_farmland + ag_production - ag_total), na.rm = TRUE)) %>% print()   ## LOOK -- max_gap 0
stopifnot("cr has no join_number -- add it to KEEP in 3_callreport.R" = "join_number" %in% names(d))
d <- d %>% mutate(join_number = as.numeric(join_number)) %>% left_join(ag, by = c("join_number", "qidx"))
AG_START <- 2017L * 4L + 3L; DQ_START <- 2022L * 4L + 1L
d <- d %>% mutate(across(c(ag_total, ag_farmland, ag_production, ag_nonmember, ag_ytd), ~ ifelse(qidx < AG_START, NA_real_, replace_na(.x, 0))),
                  across(c(ag_dq60, ag_chgoff_ytd), ~ ifelse(qidx < DQ_START, NA_real_, replace_na(.x, 0))))
cat("Farm loans matched, latest quarter:", d %>% filter(qidx == max(qidx), ag_total > 0) %>% nrow(), "credit unions with a balance\n")   ## LOOK

## ---- 6.1  eight loan types: map the panel columns ----------------------------------
## Each type is matched by pattern on the column NAME. The first match wins.
## CHECK THE PRINTED TABLE: if a type maps to the wrong column, or to nothing,
## type the right column name into LOAN_OVERRIDE and re-run this block.
LOAN_TYPES <- list(
  first_mortgage = c("^lns_re_1_tot$", "re_1st", "first_mort", "1st_mort"),
  other_re       = c("^lns_re_oth", "re_2nd", "heloc", "other_re", "junior"),
  auto           = c("^lns_auto$", "auto_tot", "new_veh", "vehicle"),
  credit_card    = c("^lns_cc$", "credit_card", "unsec_cc"),
  other_unsec    = c("unsec_oth", "other_unsec", "^lns_unsec$", "personal"),
  student        = c("student", "stud"),
  business       = c("^lns_mbl$", "^lns_comm$", "commercial", "mbl"),
  agricultural   = c("agri", "^lns_ag", "farm")
)
LOAN_OVERRIDE <- list(agricultural = "ag_total")      # farm loans come from 6.0b; add others if a row is wrong

pick <- function(pats) {
  nm <- names(d)
  for (p in pats) { hit <- grep(p, nm, ignore.case = TRUE, value = TRUE); hit <- hit[!grepl("^dq_|dq$|chg|_ct$|count|num", hit)]
                    if (length(hit)) return(hit[1]) }
  NA_character_
}
MAP <- sapply(names(LOAN_TYPES), function(k) if (!is.null(LOAN_OVERRIDE[[k]])) LOAN_OVERRIDE[[k]] else pick(LOAN_TYPES[[k]]))
print(data.frame(loan_type = names(MAP), panel_column = unname(MAP)), right = FALSE)   ## LOOK -- CHECK EVERY ROW

## Shares of total loans, in percent, latest-quarter-style per row
for (k in names(MAP)) {
  col <- MAP[[k]]
  d[[paste0("sh_", k)]] <- if (is.na(col)) NA_real_ else
    ifelse(d$lns_tot > 0, 100 * pmax(as.numeric(d[[col]]), 0) / d$lns_tot, NA_real_)
}
SH <- paste0("sh_", names(MAP))[!is.na(MAP)]
## farm loans are inside business lending, so they are left out of the coverage sum
d <- d %>% mutate(sh_mapped_total = rowSums(across(all_of(setdiff(SH, "sh_agricultural"))), na.rm = TRUE))

## Check: the mapped types should account for most of the book, never much over 100
## (business and agricultural can overlap with real estate, so a little over is possible).
d %>% filter(qidx == QZ, lns_tot > 0) %>%
  summarise(median_mapped_pct = round(median(sh_mapped_total), 1),
            pct_over_105 = round(100 * mean(sh_mapped_total > 105), 1)) %>% print()   ## LOOK -- median ~85-100

## ---- 6.2  the loan book, rural vs other, raw ---------------------------------------
## Latest four quarters, lenders only. Two views: the typical credit union
## (median share) and dollar-weighted (share of all loans in the segment).
L4 <- d %>% filter(qidx > QZ - 4, lns_tot > 0, !is.na(rural))
raw <- L4 %>%
  group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            across(all_of(SH), ~ round(mean(.x, na.rm = TRUE), 1), .names = "mean_{.col}"),
            .groups = "drop")
print(raw, width = Inf)                                                          ## LOOK -- average share of the book, %

any_lend <- L4 %>% filter(qidx == QZ) %>%
  group_by(rural) %>%
  summarise(across(all_of(SH), ~ round(100 * mean(.x > 0, na.rm = TRUE), 1), .names = "pct_any_{.col}"),
            .groups = "drop")
print(any_lend, width = Inf)                                                     ## LOOK -- share of credit unions making each kind of loan

## ---- 6.3  like-for-like: same model as RQ1 ------------------------------------------
## Each share regressed on rural, size and charter controls, state x quarter
## fixed effects, clustered by credit union and county. Estimation sample only.
est <- d %>% filter(est_sample == TRUE, lns_tot > 0, !is.na(rural)) %>%
  mutate(st = factor(state_code), qtr = factor(qidx))
ll <- bind_rows(lapply(SH, function(y) {
  m <- feols(as.formula(paste(y, "~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  ct <- coeftable(m)["rural", ]
  tibble(loan_type = sub("^sh_", "", y),
         nonrural_mean = mean(est[[y]][est$rural == 0], na.rm = TRUE),
         rural_gap_pts = ct[["Estimate"]], lo = ct[["Estimate"]] - 1.96 * ct[["Std. Error"]],
         hi = ct[["Estimate"]] + 1.96 * ct[["Std. Error"]], p = ct[["Pr(>|t|)"]])
})) %>%
  mutate(relative_pct = 100 * rural_gap_pts / nonrural_mean,
         across(c(nonrural_mean, rural_gap_pts, lo, hi, relative_pct), ~ round(.x, 2)), p = signif(p, 2)) %>%
  arrange(desc(rural_gap_pts))
print(ll, n = Inf, width = Inf)                                                  ## LOOK -- the headline table: points of the book, rural minus similar others

## Does a rural credit union make each kind of loan AT ALL? (linear probability, same model)
est <- est %>% mutate(across(all_of(SH), ~ as.integer(.x > 0), .names = "any_{.col}"))
ll_any <- bind_rows(lapply(paste0("any_", SH), function(y) {
  m <- feols(as.formula(paste(y, "~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  tibble(loan_type = sub("^any_sh_", "", y), rural_gap_pct_points = round(100 * coef(m)["rural"], 1))
}))
print(ll_any)                                                                    ## LOOK -- more / less likely to offer each

## ---- 6.4  does it hold in every size band? -------------------------------------------
by_band <- bind_rows(lapply(SH, function(y) {
  m <- feols(as.formula(paste(y, "~ rural:band + band + age_q + lid + mdi + fed + cecl_flag | st^qtr")), est, cluster = ~ cu_number + fips)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct %>% filter(grepl("^rural:band", term)) %>%
    transmute(loan_type = sub("^sh_", "", y), band = sub("rural:band", "", term), gap = round(Estimate, 2), p = signif(`Pr(>|t|)`, 2))
}))
by_band %>% select(-p) %>% pivot_wider(names_from = band, values_from = gap) %>% print(width = Inf)   ## LOOK -- same sign across bands?

## ---- 6.5  farm lending ----------------------------------------------------------------
## (a) who lends to farms -- the AgLending definition: active in the latest quarter,
##     any balance OR year-to-date farm lending in the past two years (9 quarters),
##     nonmember participations included. Should reconcile to the AgLending count
##     for the same quarter (475 at 2026Q2).
win <- (QZ - 8L):QZ
active_ag <- d %>% filter(qidx %in% win) %>%
  group_by(cu_number) %>% summarise(lends_to_farms = any(ag_total > 0 | ag_ytd > 0, na.rm = TRUE), .groups = "drop")
farm <- d %>% filter(qidx == QZ, !is.na(rural)) %>% select(cu_number, rural, fips, lns_tot, ag_total, ag_farmland, ag_nonmember) %>%
  left_join(active_ag, by = "cu_number") %>% mutate(lends_to_farms = lends_to_farms %in% TRUE)
cat("Credit unions that lend to farms (AgLending definition):", sum(farm$lends_to_farms), "\n")   ## LOOK -- vs AgLending 475 (2026Q2)
farm %>% group_by(rural) %>%
  summarise(credit_unions = n(), lend_to_farms = sum(lends_to_farms),
            pct_lend_to_farms = round(100 * mean(lends_to_farms), 1),
            farm_loans_bn = round(sum(ag_total, na.rm = TRUE) / 1e9, 2),
            pct_of_all_farm_dollars = NA_real_, .groups = "drop") %>%
  mutate(pct_of_all_farm_dollars = round(100 * farm_loans_bn / sum(farm_loans_bn), 1)) %>%
  print()                                                                        ## LOOK -- is farm lending a rural story?

## (b) among farm lenders: how big a slice of the book, how much is farmland, how much bought in
farm %>% filter(lends_to_farms, lns_tot > 0) %>% group_by(rural) %>%
  summarise(lenders = n(),
            median_farm_share_of_loans = round(median(100 * ag_total / lns_tot, na.rm = TRUE), 2),
            pct_farmland_secured = round(100 * sum(ag_farmland, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 1),
            pct_bought_from_others = round(100 * sum(ag_nonmember, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 1),
            .groups = "drop") %>% print()                                        ## LOOK -- do rural lenders originate, or buy in?

## (c) farming-dependent counties (USDA typology from maps.R MAP 9): do the
##     rural credit unions there actually lend to farms?
if (exists("typ") && "farming" %in% names(typ)) {
  farm %>% filter(rural == 1) %>% left_join(as_tibble(typ) %>% select(fips, farming), by = "fips") %>%
    group_by(farming_county = farming %in% TRUE) %>%
    summarise(credit_unions = n(), pct_lend_to_farms = round(100 * mean(lends_to_farms), 1), .groups = "drop") %>%
    print()                                                                      ## LOOK -- the policy-relevant cell
} else cat("Farming-county split skipped: run maps.R through MAP 9 first (it builds `typ`).\n")

## (d) like-for-like: is a rural credit union more likely to lend to farms than a similar one elsewhere?
lff <- d %>% filter(qidx >= AG_START, est_sample == TRUE, !is.na(rural), lns_tot > 0) %>%
  mutate(any_ag = as.integer(ag_total > 0 | ag_ytd > 0), st = factor(state_code), qtr = factor(qidx))
m_ag <- feols(as.formula(paste("any_ag ~ rural +", CTRL, "| st^qtr")), lff, cluster = ~ cu_number + fips)
cat(sprintf("Rural CUs are %+.1f percentage points more likely to hold farm loans than similar CUs (p = %s)\n",
            100 * coef(m_ag)["rural"], signif(pvalue(m_ag)["rural"], 2)))      ## LOOK

## (e) farm-loan delinquency (60+ days), rural vs other, from 2022Q1, dollar-weighted
d %>% filter(qidx >= DQ_START, !is.na(rural), ag_total > 0) %>%
  mutate(yr = (qidx - 1L) %/% 4L) %>% group_by(yr, rural) %>%
  summarise(farm_dq_rate = round(100 * sum(ag_dq60, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = rural, values_from = farm_dq_rate, names_prefix = "rural_") %>% print()   ## LOOK -- is rural farm credit riskier?

## ---- 6.6  has the gap changed over time? ---------------------------------------------
trend <- bind_rows(lapply(SH, function(y) {
  m <- feols(as.formula(paste(y, "~ rural:factor(yr) +", CTRL, "| st^qtr")),
             est %>% mutate(yr = (qidx - 1L) %/% 4L), cluster = ~ cu_number + fips)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct %>% filter(grepl("^rural:factor", term)) %>%
    transmute(loan_type = sub("^sh_", "", y), year = as.integer(gsub("\\D", "", term)), gap = Estimate)
}))
trend %>% filter(year %in% c(min(year), 2015, 2020, max(year))) %>%
  mutate(gap = round(gap, 1)) %>% pivot_wider(names_from = year, values_from = gap) %>% print()   ## LOOK -- widening or narrowing?

## ---- 6.7  credit quality by loan type ------------------------------------------------
dq_cols <- grep("^dq_", names(d), value = TRUE)
if (length(dq_cols)) {
  d %>% filter(qidx > QZ - 4, lns_tot > 0, !is.na(rural)) %>%
    group_by(rural) %>%
    summarise(across(all_of(dq_cols), ~ round(median(.x, na.rm = TRUE), 2)), .groups = "drop") %>%
    print(width = Inf)                                                           ## LOOK -- delinquency by type, median, latest year
}

## ---- 6.8  chart, CSVs, slide numbers -------------------------------------------------
LAB <- c(first_mortgage = "First mortgages", other_re = "Other real estate", auto = "Auto",
         credit_card = "Credit cards", other_unsec = "Other unsecured", student = "Student",
         business = "Business", agricultural = "Farm")
p6 <- ll %>%
  mutate(label = factor(LAB[loan_type], levels = LAB[loan_type][order(rural_gap_pts)]),
         dir = ifelse(rural_gap_pts >= 0, "More in rural books", "Less in rural books")) %>%
  ggplot(aes(x = rural_gap_pts, y = label, colour = dir)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, orientation = "y") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("More in rural books" = "#0E4C55", "Less in rural books" = "#C8553D"), name = NULL) +
  labs(title = "How the rural loan book differs from similar credit unions elsewhere",
       subtitle = "Percentage points of the loan book, rural minus similar others (same size, state, quarter); bars = 95% range",
       x = "Percentage points of total loans", y = NULL, caption = "Federally insured credit unions, assets >= $1m, 2010-2026. Preliminary.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(file.path(OUT6, "rq6_loan_mix_gap.png"), p6, width = 10, height = 5, dpi = 300, bg = "white")

write.csv(raw,     file.path(OUT6, "rq6_raw_shares.csv"),       row.names = FALSE)
write.csv(any_lend,file.path(OUT6, "rq6_any_lending.csv"),      row.names = FALSE)
write.csv(ll,      file.path(OUT6, "rq6_like_for_like.csv"),    row.names = FALSE)
write.csv(by_band, file.path(OUT6, "rq6_by_size_band.csv"),     row.names = FALSE)
write.csv(trend,   file.path(OUT6, "rq6_trend.csv"),            row.names = FALSE)
if (exists("register_result")) for (i in seq_len(nrow(ll)))
  register_result(6, paste0("gap_", ll$loan_type[i]), ll$rural_gap_pts[i], "pts of loan book, like-for-like")

cat("\n=== SLIDE NUMBERS, Q6 ===\n")
for (i in seq_len(nrow(ll))) cat(sprintf("%-18s rural %+.1f pts of the book vs similar CUs (others hold %.1f%%; p = %s)\n",
                                         LAB[ll$loan_type[i]], ll$rural_gap_pts[i], ll$nonrural_mean[i], ll$p[i]))
if ("sh_business" %in% SH)
  cat(sprintf("Rural CUs with NO business lending: %.0f%% (expectation: about two thirds)\n",
              100 - any_lend$pct_any_sh_business[any_lend$rural == 1]))
