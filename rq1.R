###############################################################################
# rq1.R  --  RESEARCH QUESTION 1
# "Are rural credit unions different because they are rural, or just because
#  they are small?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# WHAT THIS ADDS BEYOND 8_q1.R
#   8_q1 established the spec ladder, common support, matched pairs and the
#   survivorship check. This script takes the finding apart to see where it
#   could break:
#     rq1.3  does the advantage hold in EVERY size band, or is one band
#            carrying it?
#     rq1.4  is it stable over time, or a post-2020 artefact?
#     rq1.5  placebo -- shuffle the rural label inside state x quarter and see
#            where the real coefficient sits in the placebo distribution
#     rq1.6  mechanism probes -- competition, membership penetration, charter
#            type. Descriptive, not causal. Each is added to the model and we
#            watch how much of the rural coefficient it absorbs.
#     rq1.7  the numbers the slide needs, printed in one block
#
# REQUIRES: cr from 3_callreport.R (run 1_ -> 2_ -> 3_ first).
#   If you are working from the RQ1_ rebuilt panel instead, point PANEL at it;
#   the column names below are resolved by pattern and will tell you if
#   something is missing.
#
# Sequential blocks. Run each, look at what it prints, then move on.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(fixest); library(ggplot2)
})

## ---- rq1.0  panel and columns ----------------------------------------------

stopifnot(exists("cr")); if (!is.data.table(cr)) setDT(cr)
OUT <- file.path(if (exists("OUT_DIR")) OUT_DIR else "output", "rq1")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

need <- c("cu_number","qidx","fips","state_code","rural","assets_tot","ln_assets",
          "age_q","lid","mdi","fed","cecl_flag","band","members","members_pot",
          "g_assets","g_members","g_loans","roa_pct","networth_pct")
missing <- setdiff(need, names(cr))
if (length(missing)) stop("cr is missing: ", paste(missing, collapse = ", "))

OUTCOMES <- c(g_assets = "Asset growth", g_loans = "Loan growth",
              g_members = "Membership growth", roa_pct = "Return on assets",
              networth_pct = "Net worth ratio")
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"

est <- cr[!is.na(g_assets) & !is.na(rural)]
est[, `:=`(qtr = factor(qidx), st = factor(state_code))]
est[, yr := (qidx - 1L) %/% 4L]

cat(sprintf("Estimation sample: %s CU-quarters, %s CUs (%s rural)\n",
            format(nrow(est), big.mark = ","), format(uniqueN(est$cu_number), big.mark = ","),
            format(est[rural == 1, uniqueN(cu_number)], big.mark = ",")))

fit <- function(y, rhs, fe = "st^qtr", d = est)
  feols(as.formula(sprintf("%s ~ %s | %s", y, rhs, fe)), data = d,
        cluster = ~ cu_number + fips)
getb <- function(m, term = "rural") {
  ct <- coeftable(m); if (!term %in% rownames(ct)) return(c(NA, NA, NA))
  ct[term, c("Estimate", "Std. Error", "Pr(>|t|)")]
}

## ---- rq1.1  common support (must clear before anything else) ---------------

rng <- est[rural == 1, range(ln_assets)]
support <- 100 * est[rural == 0, mean(ln_assets >= rng[1] & ln_assets <= rng[2])]
cat(sprintf("Common support: %.1f%% of non-rural CU-quarters fall inside the rural size range\n", support))
if (support < 70) warning("Common support below 70% -- lead with the restricted-sample spec.")
est[, in_support := ln_assets >= rng[1] & ln_assets <= rng[2]]

## ---- rq1.2  the spec ladder, all outcomes -----------------------------------
## Same ladder as 8_q1 so the two agree. Raw -> size -> full controls ->
## state x quarter FE -> common support.

SPEC <- list(
  raw        = function(y) fit(y, "rural", "qtr"),
  size_only  = function(y) fit(y, "rural + ln_assets + I(ln_assets^2)", "qtr"),
  full_ctrl  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "qtr"),
  state_qtr  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr"),
  support    = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr", est[in_support == TRUE]))

ladder <- rbindlist(lapply(names(OUTCOMES), function(y) rbindlist(lapply(names(SPEC), function(s) {
  b <- getb(SPEC[[s]](y))
  data.table(outcome = OUTCOMES[[y]], spec = s, beta = b[1], se = b[2], p = b[3])
}))))
ladder[, spec := factor(spec, levels = names(SPEC))]
ladder_w <- dcast(ladder, outcome ~ spec, value.var = "beta")
ladder_w[, pct_surviving := round(100 * state_qtr / raw, 1)]
cat("\n=== rq1.2  Rural coefficient by specification (percentage points) ===\n")
print(ladder_w)                                                          ## LOOK
cat("pct_surviving > 100 means conditioning RAISED the gap: size was hiding the rural advantage.\n")
fwrite(ladder, file.path(OUT, "rq1_2_spec_ladder.csv"))

## ---- rq1.3  does it hold in every size band? --------------------------------
## Interact rural with band. If one band carries the whole result the report
## should say so; if all bands agree, that is the strongest single defence.

band_res <- rbindlist(lapply(names(OUTCOMES), function(y) {
  m <- fit(y, paste("rural:band + band", "age_q + lid + mdi + fed + cecl_flag", sep = " + "), "st^qtr")
  ct <- coeftable(m); rows <- grep("^rural:band", rownames(ct), value = TRUE)
  data.table(outcome = OUTCOMES[[y]],
             band = sub("^rural:band", "", rows),
             beta = ct[rows, "Estimate"], se = ct[rows, "Std. Error"], p = ct[rows, "Pr(>|t|)"])
}))
band_res[, band := factor(band, levels = levels(est$band))]
cat("\n=== rq1.3  Rural coefficient WITHIN each asset band ===\n")
print(dcast(band_res, band ~ outcome, value.var = "beta"))                  ## LOOK
band_n <- est[, .(cu_quarters = .N, rural_cus = uniqueN(cu_number[rural == 1])), by = band][order(band)]
print(band_n)                                                              ## LOOK -- the >$1B band is ~10 rural CUs
fwrite(band_res, file.path(OUT, "rq1_3_by_band.csv"))

pos_share <- band_res[outcome %in% c("Asset growth", "Return on assets"),
                      .(bands_positive = sum(beta > 0), bands_total = .N,
                        bands_signif = sum(beta > 0 & p < .05)), by = outcome]
print(pos_share)                                                           ## LOOK

F1 <- ggplot(band_res[outcome %in% c("Asset growth","Return on assets","Net worth ratio")],
             aes(band, beta, colour = outcome, group = outcome)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_pointrange(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se),
                  position = position_dodge(width = .5)) +
  labs(title = "Rural advantage within each asset-size band",
       subtitle = "State x quarter fixed effects. One band carrying the result would show as a lone positive.",
       x = NULL, y = "Rural minus non-rural, percentage points", colour = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave(file.path(OUT, "rq1_3_by_band.png"), F1, width = 10, height = 6, dpi = 200, bg = "white")

## ---- rq1.4  is it stable over time? -----------------------------------------
## Rural x year interaction. A finding that only appears after 2020 is a
## pandemic story, not a rural story.

yr_res <- rbindlist(lapply(c("g_assets","roa_pct"), function(y) {
  m <- fit(y, paste("rural:factor(yr)", CTRL, sep = " + "), "st^qtr")
  ct <- coeftable(m); rows <- grep("^rural:factor\\(yr\\)", rownames(ct), value = TRUE)
  data.table(outcome = OUTCOMES[[y]], yr = as.integer(sub(".*\\)", "", rows)),
             beta = ct[rows, "Estimate"], se = ct[rows, "Std. Error"])
}))
cat("\n=== rq1.4  Rural coefficient by year ===\n")
print(dcast(yr_res, yr ~ outcome, value.var = "beta"))                     ## LOOK
yr_res[, .(years_positive = sum(beta > 0), years = .N,
           pre2020_mean = mean(beta[yr < 2020]), post2020_mean = mean(beta[yr >= 2020])), by = outcome]  ## LOOK
fwrite(yr_res, file.path(OUT, "rq1_4_by_year.csv"))

F2 <- ggplot(yr_res, aes(yr, beta)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se), alpha = .15) +
  geom_line() + geom_point() + facet_wrap(~ outcome, scales = "free_y") +
  labs(title = "Rural advantage year by year", x = NULL, y = "Percentage points") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT, "rq1_4_by_year.png"), F2, width = 10, height = 5, dpi = 200, bg = "white")

## ---- rq1.5  placebo: shuffle the rural label ---------------------------------
## Reassign "rural" at random to the same NUMBER of CUs within each state, keep
## everything else, re-estimate. If the real coefficient sits in the middle of
## the placebo distribution, the finding is noise. It should sit far in the
## tail. 200 draws is enough to see that; raise it if the tail is close.

set.seed(909)
N_PLACEBO <- 200L
cu_state <- unique(est[, .(cu_number, state_code, rural)])
placebo <- vapply(seq_len(N_PLACEBO), function(i) {
  sh <- copy(cu_state)
  sh[, rural_p := sample(rural), by = state_code]          # same rural count per state
  d  <- est[sh[, .(cu_number, rural_p)], on = "cu_number"]
  getb(fit("g_assets", paste("rural_p", CTRL, sep = " + "), "st^qtr", d), "rural_p")[1]
}, numeric(1))
beta_real <- ladder[outcome == "Asset growth" & spec == "state_qtr", beta]
cat(sprintf("\n=== rq1.5  Placebo ===\nreal beta %.3f | placebo mean %.3f sd %.3f | share of placebos >= real: %.3f\n",
            beta_real, mean(placebo), sd(placebo), mean(placebo >= beta_real)))   ## LOOK
fwrite(data.table(draw = seq_along(placebo), beta = placebo), file.path(OUT, "rq1_5_placebo.csv"))

F3 <- ggplot(data.table(beta = placebo), aes(beta)) +
  geom_histogram(bins = 30, fill = "grey75", colour = "white") +
  geom_vline(xintercept = beta_real, colour = "#C8553D", linewidth = 1.1) +
  annotate("text", x = beta_real, y = Inf, label = "actual", vjust = 2, hjust = -.1, colour = "#C8553D") +
  labs(title = "Where the real coefficient sits against 200 random reassignments of 'rural'",
       x = "Rural coefficient on asset growth, percentage points", y = "Placebo draws") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT, "rq1_5_placebo.png"), F3, width = 9, height = 5, dpi = 200, bg = "white")

## ---- rq1.6  mechanism probes (descriptive) ----------------------------------
## "Rural" bundles several things. We cannot identify the mechanism, but we can
## see how much of the rural coefficient each candidate absorbs when added to
## the model. A large drop means the candidate is at least a plausible channel.
##
##   competition  -- number of OTHER credit unions headquartered in the county,
##                   and in the state, in that quarter
##   penetration  -- members / members_pot (how much of the potential field of
##                   membership is already served)
##   charter type -- federal vs state (already in CTRL as `fed`)

est[, `:=`(n_cus_county = .N - 1L), by = .(fips, qidx)]
est[, `:=`(n_cus_state  = .N - 1L), by = .(state_code, qidx)]
est[, penetration := fifelse(members_pot > 0, 100 * members / members_pot, NA_real_)]
est[penetration > 100, penetration := NA_real_]           # implausible -- drop rather than cap
est[, ln_ncc := log1p(n_cus_county)]

cat("\n=== rq1.6  How the candidate channels differ by rurality ===\n")
print(est[, .(cus_in_county = as.numeric(median(n_cus_county)),
              cus_in_state  = as.numeric(median(n_cus_state)),
              penetration   = as.numeric(median(penetration, na.rm = TRUE)),
              pct_federal   = 100 * mean(fed)), by = rural])              ## LOOK

mech <- rbindlist(lapply(c("g_assets","roa_pct"), function(y) {
  base <- getb(fit(y, paste("rural", CTRL, sep = " + ")))[1]
  rbindlist(list(
    data.table(outcome = OUTCOMES[[y]], added = "baseline",           beta = base),
    data.table(outcome = OUTCOMES[[y]], added = "+ competition",
               beta = getb(fit(y, paste("rural", CTRL, "ln_ncc + log1p(n_cus_state)", sep = " + ")))[1]),
    data.table(outcome = OUTCOMES[[y]], added = "+ penetration",
               beta = getb(fit(y, paste("rural", CTRL, "penetration", sep = " + "),
                               d = est[!is.na(penetration)]))[1]),
    data.table(outcome = OUTCOMES[[y]], added = "+ both",
               beta = getb(fit(y, paste("rural", CTRL, "ln_ncc + log1p(n_cus_state) + penetration", sep = " + "),
                               d = est[!is.na(penetration)]))[1])))
}))
mech[, share_absorbed := round(100 * (1 - beta / beta[added == "baseline"]), 1), by = outcome]
cat("\n=== rq1.6  Share of the rural coefficient absorbed by each candidate ===\n")
print(mech)                                                                ## LOOK
cat("Read as suggestive only. A channel that absorbs a lot is worth a paragraph;\n")
cat("none of this identifies a cause, and the report must say so.\n")
fwrite(mech, file.path(OUT, "rq1_6_mechanism_probes.csv"))

## ---- rq1.7  FOR THE SLIDE ---------------------------------------------------

sl <- ladder_w[outcome %in% c("Asset growth","Return on assets","Net worth ratio")]
cat("\n================ RQ1 -- SLIDE NUMBERS ================\n")
cat(sprintf("Common support ............. %.1f%%\n", support))
for (i in seq_len(nrow(sl)))
  cat(sprintf("%-18s raw %+.2f  like-for-like %+.3f  (%s%% of gap left)\n",
              sl$outcome[i], sl$raw[i], sl$state_qtr[i], sl$pct_surviving[i]))
cat(sprintf("Bands positive (growth) .... %d of %d\n",
            pos_share[outcome == "Asset growth", bands_positive], pos_share[outcome == "Asset growth", bands_total]))
cat(sprintf("Years positive (growth) .... %d of %d\n",
            yr_res[outcome == "Asset growth", sum(beta > 0)], yr_res[outcome == "Asset growth", .N]))
cat(sprintf("Placebo: share of draws >= actual ... %.3f (%d draws)\n", mean(placebo >= beta_real), N_PLACEBO))
cat(sprintf("Largest channel absorbed .... %s, %.0f%% of the growth coefficient\n",
            mech[outcome == "Asset growth" & added != "baseline"][which.max(share_absorbed), added],
            mech[outcome == "Asset growth" & added != "baseline", max(share_absorbed)]))
cat("======================================================\n")
