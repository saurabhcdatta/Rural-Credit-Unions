###############################################################################
# rq6.R  --  RESEARCH QUESTION 6   (v2, 9 Nov 2026)
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
# WHAT CHANGED IN v2
#   * Farm loans come from the AgLending build (account codes, see 6.0b).
#   * Business = commercial loans (lns_comm), reported from 2017Q3. The older
#     member-business-loan field (lns_mbl) stops at 2017Q2 and is not used.
#   * Other real estate = fixed-rate + adjustable-rate (v1 took fixed only).
#   * Every loan type is analysed on the SAME years, 2017Q3 on, so rows compare.
#   * Farm loans are commercial loans: shown as "of which" inside business,
#     never added to it.
#
# WHAT EACH BLOCK DOES
#   6.0  gates; read the loan columns 3_callreport.R did not keep
#   6.0b farm loans from the AgLending build
#   6.1  the seven loan types and their shares        (CHECK THE COVERAGE LINE)
#   6.2  shares of the loan book, rural vs other, raw
#   6.3  like-for-like: each share on rural, RQ1 model  (THE HEADLINE TABLE)
#   6.4  does it hold in every size band?
#   6.5  farm lending: who, how much, where, how risky
#   6.6  has the gap changed since 2017?
#   6.7  delinquency by loan type
#   6.8  chart, CSVs, slide numbers
#
# REQUIRES  cr (3_callreport.R or rq_load.R), county, PANEL_FILE, OUT_DIR,
#           CODE_DIR; the AgLending all_in.dta; for 6.5(c) maps.R run through
#           MAP 9 (builds `typ`). Packages: data.table, dplyr, tidyr, fixest,
#           ggplot2, haven.
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
AG_START <- 2017L * 4L + 3L      # 2017Q3: farm, commercial and other-RE splits begin
DQ_START <- 2022L * 4L + 1L      # 2022Q1: farm delinquency and charge-offs begin
cat("Latest quarter:", q_lab(QZ), "| analysis window:", q_lab(AG_START), "to", q_lab(QZ), "\n")

## the loan columns used below, read from the Stata panel with the keys
PANEL_LOANS <- c("lns_re_1_tot", "lns_re_1_tot_ncomm", "lns_re_oth_fr_ncomm", "lns_re_oth_ar_ncomm",
                 "lns_auto", "lns_cc", "lns_student", "lns_comm", "lns_comm_ag")
## The 2022 form split real estate into commercial and non-commercial:
##   first mortgages  lns_re_1_tot to 2021Q4, lns_re_1_tot_ncomm from 2022Q1
##   other real estate (home equity, second liens) only from 2022Q1
## Any pre-2022 other-real-estate field, if the panel has one, shows here:
print(grep("re_oth|re_jr|junior|heloc|re_2nd|equity", names(hdr), value = TRUE, ignore.case = TRUE))   ## LOOK -- send this list
hdr <- haven::read_dta(PANEL_FILE, n_max = 0)
dq_panel <- grep("^dq_", names(hdr), value = TRUE)
print(setdiff(PANEL_LOANS, names(hdr)))                                          ## LOOK -- should be empty
want <- intersect(c(PANEL_LOANS, dq_panel), names(hdr))
extra <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(c("cu_number", "year", "quarter", want)))
extra <- haven::zap_labels(extra) %>%
  mutate(cu_number = as.numeric(cu_number), qidx = as.integer(year) * 4L + as.integer(quarter)) %>%
  select(-year, -quarter)
extra <- extra[, c("cu_number", "qidx", setdiff(names(extra), c(names(cr), "cu_number", "qidx")))]
d <- cr %>% as_tibble() %>% left_join(extra, by = c("cu_number", "qidx"))
cat("Panel rows:", format(nrow(d), big.mark = ","), "| loan columns added:", ncol(extra) - 2, "\n")

## ---- 6.0b  farm loans: the AgLending build --------------------------------------------
## Farm loans come from the 5300 report database (FS220L/M/P), pulled by the
## AgLending Stata job and saved as all_in.dta (join_number x Stata quarter).
##   042A5 member, secured by farmland        042A6 member, production & other farm
##   042A7 nonmember participations, farmland 042A8 nonmember participations, production
##   042A9 total agricultural commercial loans (= A5+A6+A7+A8)
##   463A5-A8 YTD dollars granted/purchased   DL0090/DL0118 delinquent 60+ days
##   CH0025/CH0033 YTD charge-offs
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
ag %>% summarise(rows = n(), max_gap = max(abs(ag_farmland + ag_production - ag_total), na.rm = TRUE)) %>% print()   ## LOOK -- max_gap 0
stopifnot("cr has no join_number -- add it to KEEP in 3_callreport.R" = "join_number" %in% names(d))
d <- d %>% mutate(join_number = as.numeric(join_number)) %>% left_join(ag, by = c("join_number", "qidx")) %>%
  mutate(across(c(ag_total, ag_farmland, ag_production, ag_nonmember, ag_ytd), ~ ifelse(qidx < AG_START, NA_real_, replace_na(.x, 0))),
         across(c(ag_dq60, ag_chgoff_ytd), ~ ifelse(qidx < DQ_START, NA_real_, replace_na(.x, 0))))
## cross-check against the panel's own commercial-ag field (should be close)
if ("lns_comm_ag" %in% names(d))
  d %>% filter(qidx == QZ) %>%
    summarise(ag_build_bn = round(sum(ag_total, na.rm = TRUE) / 1e9, 2),
              panel_lns_comm_ag_bn = round(sum(as.numeric(lns_comm_ag), na.rm = TRUE) / 1e9, 2)) %>% print()   ## LOOK -- close?

## ---- 6.1  seven loan types and their shares ------------------------------------------
## From here on the analysis uses 2017Q3 onward only, so every type covers the
## same years. Farm loans are a slice of business (commercial) lending.
d <- d %>% filter(qidx >= AG_START) %>%
  mutate(first_mortgage = coalesce(as.numeric(lns_re_1_tot), as.numeric(lns_re_1_tot_ncomm)),   # bridged across 2022
         other_re       = ifelse(is.na(lns_re_oth_fr_ncomm) & is.na(lns_re_oth_ar_ncomm), NA_real_,
                                 coalesce(as.numeric(lns_re_oth_fr_ncomm), 0) + coalesce(as.numeric(lns_re_oth_ar_ncomm), 0)),
         auto           = as.numeric(lns_auto),
         credit_card    = as.numeric(lns_cc),
         student        = as.numeric(lns_student),
         business       = as.numeric(lns_comm),
         agricultural   = ag_total)
TYPES <- c("first_mortgage", "other_re", "auto", "credit_card", "student", "business", "agricultural")
for (k in TYPES) d[[paste0("sh_", k)]] <- ifelse(d$lns_tot > 0, 100 * pmax(d[[k]], 0) / d$lns_tot, NA_real_)
SH <- paste0("sh_", TYPES)

## continuity check across the 2022 switch: the average first-mortgage share should
## move smoothly from 2021 to 2022, not jump (a jump = the two fields measure different things)
d %>% filter(lns_tot > 0, qidx >= 2020L * 4L + 1L, qidx <= 2023L * 4L + 4L) %>%
  group_by(quarter = q_lab(qidx)) %>%
  summarise(mean_first_mortgage_share = round(mean(sh_first_mortgage, na.rm = TRUE), 1),
            pct_reported = round(100 * mean(!is.na(first_mortgage)), 1), .groups = "drop") %>% print(n = Inf)   ## LOOK -- smooth across 2021Q4 -> 2022Q1?

## coverage: the six types (farm excluded -- it is inside business) should be most
## of the book. The remainder is mainly unsecured personal loans and leases.
d <- d %>% mutate(sh_covered = rowSums(across(all_of(setdiff(SH, "sh_agricultural"))), na.rm = TRUE))
d %>% filter(qidx == QZ, lns_tot > 0) %>%
  summarise(median_covered_pct = round(median(sh_covered), 1), pct_over_105 = round(100 * mean(sh_covered > 105), 1)) %>%
  print()                                                                        ## LOOK -- median ~85-95, almost none over 105
d %>% filter(qidx == QZ, lns_tot > 0) %>%
  summarise(across(all_of(TYPES), ~ round(100 * mean(!is.na(.x)), 1), .names = "pct_reported_{.col}")) %>%
  print(width = Inf)                                                             ## LOOK -- every type ~100

## ---- 6.2  the loan book, rural vs other, raw ---------------------------------------
L4 <- d %>% filter(qidx > QZ - 4, lns_tot > 0, !is.na(rural))
raw <- L4 %>% group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            across(all_of(SH), ~ round(mean(.x, na.rm = TRUE), 1), .names = "mean_{.col}"), .groups = "drop")
print(raw, width = Inf)                                                          ## LOOK -- average share of the book, %, latest year
any_lend <- L4 %>% filter(qidx == QZ) %>% group_by(rural) %>%
  summarise(across(all_of(SH), ~ round(100 * mean(.x > 0, na.rm = TRUE), 1), .names = "pct_any_{.col}"), .groups = "drop")
print(any_lend, width = Inf)                                                      ## LOOK -- share of CUs making each kind of loan

## ---- 6.3  like-for-like: same model as RQ1 ------------------------------------------
est <- d %>% filter(est_sample == TRUE, lns_tot > 0, !is.na(rural)) %>%
  mutate(st = factor(state_code), qtr = factor(qidx))
cat("Estimation sample:", format(nrow(est), big.mark = ","), "CU-quarters,", q_lab(min(est$qidx)), "to", q_lab(max(est$qidx)), "\n")
ll <- bind_rows(lapply(SH, function(y) {
  m <- feols(as.formula(paste(y, "~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  ct <- coeftable(m)["rural", ]
  tibble(loan_type = sub("^sh_", "", y),
         nonrural_mean = mean(est[[y]][est$rural == 0], na.rm = TRUE),
         rural_mean    = mean(est[[y]][est$rural == 1], na.rm = TRUE),
         rural_gap_pts = ct[["Estimate"]], lo = ct[["Estimate"]] - 1.96 * ct[["Std. Error"]],
         hi = ct[["Estimate"]] + 1.96 * ct[["Std. Error"]], p = ct[["Pr(>|t|)"]])
})) %>%
  mutate(relative_pct = 100 * rural_gap_pts / nonrural_mean,
         across(c(nonrural_mean, rural_mean, rural_gap_pts, lo, hi, relative_pct), ~ round(.x, 2)), p = signif(p, 2)) %>%
  arrange(desc(rural_gap_pts))
print(ll, n = Inf, width = Inf)                                                  ## LOOK -- the headline table
## sanity: farm gap must be smaller than the business gap it sits inside
cat("Check -- farm gap <= business gap:",
    ll$rural_gap_pts[ll$loan_type == "agricultural"] <= ll$rural_gap_pts[ll$loan_type == "business"], "\n")   ## LOOK -- TRUE

est <- est %>% mutate(across(all_of(SH), ~ as.integer(.x > 0), .names = "any_{.col}"))
ll_any <- bind_rows(lapply(paste0("any_", SH), function(y) {
  m <- feols(as.formula(paste(y, "~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  tibble(loan_type = sub("^any_sh_", "", y), rural_gap_pct_points = round(100 * coef(m)["rural"], 1),
         p = signif(pvalue(m)["rural"], 2))
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

## ---- 6.5  farm lending -------------------------------------------------------------
## (a) who lends to farms -- AgLending definition: active now, any balance OR YTD
##     farm lending in the past two years (9 quarters), nonmembers included.
win <- (QZ - 8L):QZ
active_ag <- d %>% filter(qidx %in% win) %>% group_by(cu_number) %>%
  summarise(lends_to_farms = any(ag_total > 0 | ag_ytd > 0, na.rm = TRUE), .groups = "drop")
farm <- d %>% filter(qidx == QZ, !is.na(rural)) %>%
  select(cu_number, rural, fips, state_code, assets_tot, lns_tot, ag_total, ag_farmland, ag_nonmember) %>%
  left_join(active_ag, by = "cu_number") %>% mutate(lends_to_farms = lends_to_farms %in% TRUE)
cat("Credit unions that lend to farms:", sum(farm$lends_to_farms), "(AgLending: 475 at 2026Q2)\n")   ## LOOK
farm_who <- farm %>% group_by(rural) %>%
  summarise(credit_unions = n(), lend_to_farms = sum(lends_to_farms),
            pct_lend_to_farms = round(100 * mean(lends_to_farms), 1),
            farm_loans_bn = round(sum(ag_total, na.rm = TRUE) / 1e9, 2), .groups = "drop") %>%
  mutate(pct_of_all_farm_dollars = round(100 * farm_loans_bn / sum(farm_loans_bn), 1),
         pct_of_all_farm_lenders = round(100 * lend_to_farms / sum(lend_to_farms), 1))
print(farm_who)                                                                  ## LOOK -- is farm lending a rural story?

## (b) among farm lenders: slice of the book, farmland share, bought in
farm_how <- farm %>% filter(lends_to_farms, lns_tot > 0) %>% group_by(rural) %>%
  summarise(lenders = n(),
            median_farm_share_of_loans = round(median(100 * ag_total / lns_tot, na.rm = TRUE), 2),
            mean_farm_share_of_loans   = round(mean(100 * ag_total / lns_tot, na.rm = TRUE), 2),
            pct_farmland_secured   = round(100 * sum(ag_farmland, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 1),
            pct_bought_from_others = round(100 * sum(ag_nonmember, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 1),
            .groups = "drop")
print(farm_how)                                                                  ## LOOK -- originate locally, or buy in?

## (c) rural CUs in farming-dependent counties (USDA typology, maps.R MAP 9)
if (exists("typ") && "farming" %in% names(typ)) {
  farm <- farm %>% left_join(as_tibble(typ) %>% select(fips, farming), by = "fips") %>% mutate(farming = farming %in% TRUE)
  farm %>% group_by(rural, farming_county = farming) %>%
    summarise(credit_unions = n(), pct_lend_to_farms = round(100 * mean(lends_to_farms), 1),
              farm_loans_m = round(sum(ag_total, na.rm = TRUE) / 1e6, 1), .groups = "drop") %>% print()   ## LOOK
} else cat("Farming-county split skipped: run maps.R through MAP 9 first (it builds `typ`).\n")

## (d) like-for-like: more likely to lend to farms than a similar CU elsewhere?
est <- est %>% mutate(any_ag = as.integer(ag_total > 0 | ag_ytd > 0))
m_ag <- feols(as.formula(paste("any_ag ~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
cat(sprintf("Rural CUs are %+.1f percentage points more likely to lend to farms than similar CUs (p = %s)\n",
            100 * coef(m_ag)["rural"], signif(pvalue(m_ag)["rural"], 2)))      ## LOOK

## (e) farm-loan delinquency (60+ days), dollar-weighted, from 2022Q1
farm_dq <- d %>% filter(qidx >= DQ_START, !is.na(rural), ag_total > 0) %>%
  mutate(yr = (qidx - 1L) %/% 4L) %>% group_by(yr, rural) %>%
  summarise(farm_dq_rate = round(100 * sum(ag_dq60, na.rm = TRUE) / sum(ag_total, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = rural, values_from = farm_dq_rate, names_prefix = "rural_")
print(farm_dq)                                                                   ## LOOK -- is rural farm credit riskier?

## ---- 6.6  has the gap changed since 2017? --------------------------------------------
trend <- bind_rows(lapply(SH, function(y) {
  m <- feols(as.formula(paste(y, "~ rural:factor(yr) +", CTRL, "| st^qtr")),
             est %>% mutate(yr = (qidx - 1L) %/% 4L), cluster = ~ cu_number + fips)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct %>% filter(grepl("^rural:factor", term)) %>%
    transmute(loan_type = sub("^sh_", "", y), year = as.integer(gsub("\\D", "", term)), gap = Estimate)
}))
trend %>% mutate(gap = round(gap, 1)) %>% pivot_wider(names_from = year, values_from = gap) %>% print(width = Inf)   ## LOOK -- widening or narrowing?

## ---- 6.7  delinquency by loan type, latest year, median --------------------------------
dq_cols <- intersect(grep("^dq_", names(d), value = TRUE), grep("rate", names(d), value = TRUE))
if (length(dq_cols))
  d %>% filter(qidx > QZ - 4, lns_tot > 0, !is.na(rural)) %>% group_by(rural) %>%
    summarise(across(all_of(dq_cols), ~ round(median(.x, na.rm = TRUE), 2)), .groups = "drop") %>%
    print(width = Inf)                                                           ## LOOK

## ---- 6.8  chart, CSVs, slide numbers -------------------------------------------------
LAB <- c(first_mortgage = "First mortgages", other_re = "Home equity & other real estate (2022 on)", auto = "Auto",
         credit_card = "Credit cards", student = "Student", business = "Business (commercial)",
         agricultural = "  of which: farm")
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
       x = "Percentage points of total loans", y = NULL,
       caption = sprintf("Federally insured credit unions, assets >= $1m, %s-%s. Farm loans are part of business lending. Preliminary.", q_lab(AG_START), q_lab(QZ))) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(file.path(OUT6, "rq6_loan_mix_gap.png"), p6, width = 10, height = 5, dpi = 300, bg = "white")

write.csv(raw,      file.path(OUT6, "rq6_raw_shares.csv"),     row.names = FALSE)
write.csv(any_lend, file.path(OUT6, "rq6_any_lending.csv"),    row.names = FALSE)
write.csv(ll,       file.path(OUT6, "rq6_like_for_like.csv"),  row.names = FALSE)
write.csv(ll_any,   file.path(OUT6, "rq6_offers_each.csv"),    row.names = FALSE)
write.csv(by_band,  file.path(OUT6, "rq6_by_size_band.csv"),   row.names = FALSE)
write.csv(trend,    file.path(OUT6, "rq6_trend.csv"),          row.names = FALSE)
write.csv(farm_who, file.path(OUT6, "rq6_farm_who.csv"),       row.names = FALSE)
write.csv(farm_how, file.path(OUT6, "rq6_farm_how.csv"),       row.names = FALSE)
write.csv(farm_dq,  file.path(OUT6, "rq6_farm_delinquency.csv"), row.names = FALSE)
write.csv(farm,     file.path(OUT6, "rq6_farm_lenders_latest.csv"), row.names = FALSE)   # for map 17
if (exists("register_result")) for (i in seq_len(nrow(ll)))
  register_result(6, paste0("gap_", ll$loan_type[i]), ll$rural_gap_pts[i], "pts of loan book, like-for-like, 2017Q3 on")

cat("\n=== SLIDE NUMBERS, Q6 ===\n")
for (i in seq_len(nrow(ll))) cat(sprintf("%-32s rural %+.1f pts vs similar CUs (others hold %.1f%%, rural %.1f%%; p = %s)\n",
                                         LAB[ll$loan_type[i]], ll$rural_gap_pts[i], ll$nonrural_mean[i], ll$rural_mean[i], ll$p[i]))
cat(sprintf("Rural CUs with NO business lending: %.0f%% (expectation: about two thirds)\n",
            100 - any_lend$pct_any_sh_business[any_lend$rural == 1]))
cat(sprintf("Farm: %d rural CUs lend to farms (%.0f%% of rural); they hold %.0f%% of CU farm-loan dollars; %+.1f pts more likely than similar CUs\n",
            farm_who$lend_to_farms[farm_who$rural == 1], farm_who$pct_lend_to_farms[farm_who$rural == 1],
            farm_who$pct_of_all_farm_dollars[farm_who$rural == 1], 100 * coef(m_ag)["rural"]))
