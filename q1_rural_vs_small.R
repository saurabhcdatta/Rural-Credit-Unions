###############################################################################
# Rural Credit Unions Study (ROAD Act Sec. 909, P.L. 119-101)
#
# EXPLORATORY QUESTION 1: Is "rural" different from "small"?
#
# Tests whether the rural gap in the three statutory outcomes -- growth,
# capital adequacy, profitability -- survives conditioning on asset size.
#
# DECISION RULE, pre-committed BEFORE running:
#   Gap largely disappears once size is accounted for
#       -> reorient the report around SCALE; cost function becomes centerpiece
#   Gap survives conditioning
#       -> rural-specific barriers become centerpiece
#
# Rural definition: 12 CFR 1026.35(b)(2)(iv)(A)(1) via the 2024 USDA-ERS Urban
# Influence Codes -- see rural_definition.R. Validated against ERS published
# county counts (1,556 rural counties, 50 states + DC).
#
# NOTE: the panel is named `cr`, NOT `dt`. `dt` masks stats::dt (the Student's
# t density) and yields "object of type 'closure' is not subsettable" whenever
# the object is missing.
###############################################################################

library(data.table)
library(fixest)

source("panel_prep.R")   # loading, rural classification, filters, outcomes

## ---------------------------------------------------------------------------
## 0. PREP
## ---------------------------------------------------------------------------
## Run exhibits_descriptive.R FIRST. The descriptive tables frame the question
## this script answers; reading the regressions without them inverts the order
## in which the report has to be argued.

CFG <- list(
  panel_dir = "path/to/dta",     # folder holding OCE_CallReport_*.dta
  cache_dir = "data/raw",
  out_dir   = "out/q1"
)
dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)

P        <- prep_panel(CFG$panel_dir, cache_dir = CFG$cache_dir)
cr       <- P$cr
uic      <- P$uic
OUTCOMES <- P$outcomes

est <- cr[acq_window == 0 & !is.na(g_assets) & !is.na(networth_pct) & !is.na(roa_pct)]
message("Estimation sample: ", nrow(est), " CU-quarters, ",
        uniqueN(est$cu_number), " credit unions (",
        est[rural == 1, uniqueN(cu_number)], " rural)")

## ---------------------------------------------------------------------------
## 5. RECLASSIFICATION EXPOSURE
## ---------------------------------------------------------------------------
## How many institutions sit in the 169 counties that changed rural status
## between UIC vintages? A handful means reclassification is a footnote; a
## hundred means it belongs in the executive summary.

reclass <- est[rural_2013 != rural_2024,
               .(credit_unions = uniqueN(cu_number), cu_quarters = .N),
               by = .(moved = fifelse(rural_2013 == 1L, "lost rural status",
                                                        "gained rural status"))]
cat("\n=== Reclassification exposure (UIC vintage change only) ===\n")
print(reclass)
cat("These institutions change category with no change in their business.\n\n")
fwrite(reclass, file.path(CFG$out_dir, "00_reclassification_exposure.csv"))

## ---------------------------------------------------------------------------
## 6. COMMON SUPPORT  -- read this BEFORE any regression
## ---------------------------------------------------------------------------
## If rural CUs occupy a size range non-rural CUs barely populate, section 8
## is extrapolating rather than comparing.

supp <- est[, .(n_cu = uniqueN(cu_number), n_obs = .N,
                p10 = quantile(assets_tot, .10), p50 = quantile(assets_tot, .50),
                p90 = quantile(assets_tot, .90), max = max(assets_tot)), by = rural]
print(supp)

rng <- est[rural == 1, range(ln_assets)]
overlap <- est[rural == 0, mean(ln_assets >= rng[1] & ln_assets <= rng[2])]
cat(sprintf("\nShare of NON-rural CU-quarters inside the rural size range: %.1f%%\n",
            100 * overlap))
cat("Below ~70%: treat m4 with suspicion and lead with m5 (common support).\n\n")
fwrite(supp, file.path(CFG$out_dir, "01_common_support.csv"))

## ---------------------------------------------------------------------------
## 7. ANALYSIS A: unconditional gap, then the SAME gap within size bands
## ---------------------------------------------------------------------------
## This is the heart of Q1 and needs no econometrics to read.

## `band` is constructed in panel_prep.R

A1 <- est[, c(.(n = .N), lapply(.SD, median, na.rm = TRUE)), by = rural, .SDcols = OUTCOMES]
A2 <- est[, c(.(n = .N), lapply(.SD, median, na.rm = TRUE)), by = .(band, rural), .SDcols = OUTCOMES]
setorder(A2, band, rural)
A3 <- est[, lapply(.SD, function(x) weighted.mean(x, assets_tot, na.rm = TRUE)),
          by = rural, .SDcols = OUTCOMES]

## The readable version: rural minus non-rural, within band
A2gap <- dcast(melt(A2, id.vars = c("band", "rural"), measure.vars = OUTCOMES),
               band + variable ~ rural, value.var = "value")
setnames(A2gap, c("0", "1"), c("nonrural", "rural"))
A2gap[, gap := rural - nonrural]

cat("\n=== Pooled (misleading -- confounds size) ===\n"); print(A1)
cat("\n=== Asset-weighted pooled ===\n");                 print(A3)
cat("\n=== Within size band (the honest version) ===\n"); print(A2gap[order(variable, band)])

fwrite(A1,    file.path(CFG$out_dir, "02_gap_pooled.csv"))
fwrite(A2,    file.path(CFG$out_dir, "03_gap_by_size_band.csv"))
fwrite(A2gap, file.path(CFG$out_dir, "03b_gap_by_band_wide.csv"))
fwrite(A3,    file.path(CFG$out_dir, "04_gap_assetweighted.csv"))

## READ A2gap FIRST. If `gap` shrinks toward zero moving across bands, size is
## doing the work -- that is the answer to Q1, and the rest is confirmation.

## ---------------------------------------------------------------------------
## 8. ANALYSIS B: conditional regressions
## ---------------------------------------------------------------------------
## FIXED EFFECTS NOTE: rural is defined by HQ county, so county FE would
## absorb it entirely and beta would not be identified. State x quarter is the
## tightest feasible specification under a county-based rural definition.

CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"

fit <- function(y, rhs, fe, data) {
  feols(as.formula(sprintf("%s ~ %s | %s", y, rhs, fe)),
        data = data, cluster = ~ cu_number + fips)
}

specs <- list(
  m1_raw   = function(y) fit(y, "rural", "qtr", est),
  m2_size  = function(y) fit(y, "rural + ln_assets + I(ln_assets^2)", "qtr", est),
  m3_full  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "qtr", est),
  m4_state = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr", est),
  m5_supp  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr",
                             est[ln_assets >= rng[1] & ln_assets <= rng[2]])
)

res <- rbindlist(lapply(names(specs), function(s)
  rbindlist(lapply(OUTCOMES, function(y) {
    m  <- specs[[s]](y)
    ct <- coeftable(m)["rural", ]
    data.table(spec = s, outcome = y, beta = ct[1], se = ct[2], p = ct[4], n = nobs(m))
  }))))
res[, stars := cut(p, c(-Inf, .01, .05, .10, Inf), labels = c("***", "**", "*", ""))]

cat("\n=== Rural coefficient by specification ===\n")
print(dcast(res, outcome ~ spec, value.var = "beta"))
fwrite(res, file.path(CFG$out_dir, "05_conditional_gap.csv"))

## The headline number: how much of the raw gap survives full conditioning.
shrink <- dcast(res, outcome ~ spec, value.var = "beta")
shrink[, pct_surviving := 100 * m4_state / m1_raw]
cat("\n=== Share of the raw gap surviving conditioning ===\n"); print(shrink)
fwrite(shrink, file.path(CFG$out_dir, "06_gap_shrinkage.csv"))

## ---------------------------------------------------------------------------
## 9. ANALYSIS C: does the rural gap VARY BY SIZE?
## ---------------------------------------------------------------------------
## If the rural penalty is concentrated among small CUs, then "rural" and
## "small" interact rather than compete, and the report must say so. This is
## the most likely real-world answer.

inter <- rbindlist(lapply(c("g_assets", "networth_pct", "roa_pct"), function(y) {
  m  <- feols(as.formula(sprintf(
          "%s ~ rural * band + age_q + lid + mdi + fed + cecl_flag | st^qtr", y)),
          data = est, cluster = ~ cu_number + fips)
  ct <- coeftable(m)
  k  <- grep("rural", rownames(ct))
  data.table(outcome = y, term = rownames(ct)[k],
             beta = ct[k, 1], se = ct[k, 2], p = ct[k, 4])
}))
cat("\n=== Rural x size-band interaction ===\n"); print(inter)
fwrite(inter, file.path(CFG$out_dir, "07_rural_by_size_interaction.csv"))

## ---------------------------------------------------------------------------
## 10. ANALYSIS D: Oaxaca-Blinder decomposition
## ---------------------------------------------------------------------------
## Splits the raw gap into:
##   EXPLAINED   -- rural CUs differ in observable characteristics (size, etc.)
##   UNEXPLAINED -- rural CUs perform differently GIVEN those characteristics
## The unexplained component is the policy-relevant object and belongs in the
## executive summary.

ob_decomp <- function(d, y, xvars) {
  f  <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  d0 <- d[rural == 0]; d1 <- d[rural == 1]
  b0 <- coef(lm(f, d0)); b1 <- coef(lm(f, d1))
  X0 <- colMeans(model.matrix(f, d0)); X1 <- colMeans(model.matrix(f, d1))
  gap <- sum(X0 * b0) - sum(X1 * b1)
  data.table(outcome = y, raw_gap = gap,
             explained   = sum((X0 - X1) * b1),
             unexplained = sum(X1 * (b0 - b1)),
             pct_unexplained = 100 * sum(X1 * (b0 - b1)) / gap)
}

XV <- c("ln_assets", "I(ln_assets^2)", "age_q", "lid", "mdi", "fed", "cecl_flag")
D  <- rbindlist(lapply(c("g_assets", "networth_pct", "roa_pct"),
                       function(y) ob_decomp(est, y, XV)))
cat("\n=== Oaxaca-Blinder decomposition ===\n"); print(D)
fwrite(D, file.path(CFG$out_dir, "08_oaxaca_decomposition.csv"))

## ---------------------------------------------------------------------------
## 11. READ-OUT
## ---------------------------------------------------------------------------

cat("\n", strrep("=", 74), "\n  Q1 READ-OUT: is 'rural' different from 'small'?\n",
    strrep("=", 74), "\n", sep = "")
cat("\nCommon support (non-rural inside rural size range): ",
    sprintf("%.1f%%\n", 100 * overlap))
cat("\nShare of raw gap surviving full conditioning:\n"); print(shrink[, .(outcome, pct_surviving)])
cat("\nShare of gap UNEXPLAINED by observables:\n");      print(D[, .(outcome, pct_unexplained)])
cat("\nDECISION:\n")
cat("  Small surviving share + small unexplained share -> SCALE story.\n")
cat("     Promote the cost function / minimum efficient scale work.\n")
cat("  Large surviving share -> RURAL-SPECIFIC story.\n")
cat("     Promote the regulatory barrier modules (Sec. 909(c)(2)).\n")
cat("  Differs by outcome, or concentrated in the small bands (section 9)\n")
cat("     -> report per-outcome; this is the likely realistic result.\n")
cat(strrep("=", 74), "\n")

saveRDS(est, file.path(CFG$out_dir, "q1_estimation_sample.rds"))
message("Outputs written to ", normalizePath(CFG$out_dir))
