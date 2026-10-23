## =============================================================================
## 12_q2_v2_2026-10-23.R   (v2: universe gate + 12.3c rural entrants list + 12.7c rural exits list)
## Q2 — Are we losing RURAL credit unions, or are we just losing credit unions?
##
## Rural Credit Unions Study | ROAD Act (P.L. 119-101) Sec. 909
## NCUA Office of the Chief Economist
##
## RUN AFTER: 1_setup.R -> 2_rural_counties.R -> 3_callreport.R
## Requires in environment: cr (5300 panel with rural classification),
##                          uic24 / uic13 (county rural universe), OUT_DIR
##
## Exploratory. Run block by block. Objects are left in the global environment.
##
## What this script establishes, in order:
##   12.1  column resolution and the CU spine
##   12.2  charter counts and indexed trend, both vintages   -> Q2_1, Q2_2
##   12.3  the flow decomposition and its accounting identity -> Q2_3
##   12.3c the list of rural entrants, one row per CU         -> Q2_3c
##   12.4  entry and exit RATES per 100 charters              -> Q2_4
##   12.5  survival — Kaplan-Meier and Cox                    -> Q2_5
##   12.6  exit composition by outcome x reason (UNVERIFIED)  -> Q2_6
##   12.7  where exiting rural charters go (acquirer rurality)-> Q2_7
##   12.7c the list of rural exits, one row per CU            -> Q2_7c
##   12.8  charter-level HQ relocation across the rural line  -> Q2_8
##   12.9  county coverage: rural counties with a local HQ    -> Q2_9
##   12.10 write exhibits, print the Q2 answer
##
## THE Q2 CLAIM THIS SCRIPT IS TESTING:
##   Charter counts fall everywhere. Rural is only a distinct problem if rural
##   charters fall FASTER. Sections 12.2-12.5 answer that. Sections 12.6-12.9
##   answer the follow-on: even if rural charters fall no faster, does the rural
##   footprint change hands, move away, or leave counties uncovered?
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
})

## ---- 12.0  guards ----------------------------------------------------------

stopifnot(exists("cr"))
if (!is.data.table(cr)) setDT(cr)

## Universe gate: charter types 1-2 only. universe.R is the ONE place this is
## defined; assert_universe() stops the script if a type-3 credit union is in cr.
## (v1 of this script had no gate, which is how the 48 could include type 3s.)
if (!exists("assert_universe")) source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "universe.R"))
assert_universe(cr, "12_q2.R")

Q2_DIR <- file.path(if (exists("OUT_DIR")) OUT_DIR else "output", "exhibits")
dir.create(Q2_DIR, recursive = TRUE, showWarnings = FALSE)

Q2 <- list()   # every exhibit lands here as well as on disk

## Resolve a column by pattern rather than by literal name. Column names have
## drifted between builds on this project (rural_site_13 vs rural_site_2013,
## q vs quarter, statecty vs statecty_fips2) — do not hardcode.
pick_col <- function(DT, patterns, what, required = TRUE) {
  nm <- names(DT)
  for (p in patterns) {
    hit <- grep(p, nm, ignore.case = TRUE, value = TRUE)
    if (length(hit)) {
      if (length(hit) > 1L)
        message(sprintf("  [%s] matched %d cols (%s) — using '%s'",
                        what, length(hit), paste(hit, collapse = ", "), hit[1]))
      return(hit[1])
    }
  }
  if (required)
    stop(sprintf("Could not resolve column for '%s'. Tried: %s\nAvailable: %s",
                 what, paste(patterns, collapse = " | "),
                 paste(nm, collapse = ", ")))
  NA_character_
}

message("Resolving columns in `cr` ...")
C <- list(
  id      = pick_col(cr, c("^cu_number$", "^cu_num", "^cuno"),        "cu id"),
  join    = pick_col(cr, c("^join_number$", "^join_num"),             "join number", FALSE),
  year    = pick_col(cr, c("^year$", "^yr$"),                         "year"),
  qtr     = pick_col(cr, c("^quarter$", "^qtr$", "^q$"),              "quarter"),
  qper    = pick_col(cr, c("^q_period$", "^cycle", "^period"),        "period", FALSE),
  assets  = pick_col(cr, c("^assets_tot$", "^assets_total$", "^assets"), "total assets"),
  members = pick_col(cr, c("^members$"),                              "members"),
  nw      = pick_col(cr, c("^networth$", "^net_worth$"),              "net worth ratio"),
  roa     = pick_col(cr, c("^roa$"),                                  "roa"),
  st      = pick_col(cr, c("^state_code$", "^state$"),                "state"),
  outcome = pick_col(cr, c("^outcome$"),                              "outcome code", FALSE),
  reason  = pick_col(cr, c("^reason$"),                               "reason code",  FALSE),
  ptr     = pick_col(cr, c("^join_number_pointer$", "pointer"),       "successor pointer", FALSE),
  lid     = pick_col(cr, c("^limited_inc$", "^lid$"),                 "LID flag", FALSE),
  name    = pick_col(cr, c("^cu_name$", "^name$"),                    "CU name", FALSE),
  ctype   = pick_col(cr, c("^cu_type$"),                              "charter type", FALSE)
)

## FIPS: use it if 3_callreport.R already built it, otherwise assemble it.
fips_col <- pick_col(cr, c("^fips5$", "^fips$", "^statecty"), "county fips", FALSE)
if (is.na(fips_col)) {
  cty_col <- pick_col(cr, c("^county_code$", "^county_fips$", "^county$"), "county code")
  cr[, fips5 := sprintf("%02d%03d", as.integer(get(C$st)), as.integer(get(cty_col)))]
  fips_col <- "fips5"
  message("  [fips] assembled from state_code + county_code")
}
C$fips <- fips_col

## Rural flags, fixed vintage. 3_callreport.R writes one per vintage.
C$r24 <- pick_col(cr, c("rural.*24", "rural.*2024", "^rural$"), "rural flag, 2024 vintage")
C$r13 <- pick_col(cr, c("rural.*13", "rural.*2013"),            "rural flag, 2013 vintage", FALSE)

str(C)

## Orderable quarter index. Nothing downstream should sort on a string.
cr[, qidx := as.integer(get(C$year)) * 4L + as.integer(get(C$qtr))]
QMIN <- min(cr$qidx, na.rm = TRUE)
QMAX <- max(cr$qidx, na.rm = TRUE)
qlab <- function(i) sprintf("%dQ%d", (i - 1L) %/% 4L, ((i - 1L) %% 4L) + 1L)
cat(sprintf("Panel spans %s to %s (%d quarters), %s CU-quarters, %s CUs\n",
            qlab(QMIN), qlab(QMAX), uniqueN(cr$qidx),
            format(nrow(cr), big.mark = ","),
            format(uniqueN(cr[[C$id]]), big.mark = ",")))

## ---- 12.1  the CU spine ----------------------------------------------------
## One row per CU-quarter, only the fields Q2 needs. Rural flags carried under
## fixed names so every block below is vintage-agnostic.

sp <- cr[, list(
  id      = get(C$id),
  qidx    = qidx,
  st      = get(C$st),
  fips    = get(C$fips),
  r24     = as.integer(get(C$r24)),
  r13     = if (!is.na(C$r13)) as.integer(get(C$r13)) else NA_integer_,
  assets  = as.numeric(get(C$assets)),
  members = as.numeric(get(C$members)),
  nw      = as.numeric(get(C$nw)),
  roa     = as.numeric(get(C$roa))
)]
if (!is.na(C$join))    sp[, join    := cr[[C$join]]]
if (!is.na(C$outcome)) sp[, outcome := cr[[C$outcome]]]
if (!is.na(C$reason))  sp[, reason  := cr[[C$reason]]]
if (!is.na(C$ptr))     sp[, ptr     := cr[[C$ptr]]]
setkey(sp, id, qidx)

## Spell endpoints per CU. Exits are right-censored at QMAX; entries at QMIN are
## left-truncated (the panel starting is not a charter being granted).
spell <- sp[, list(
  first_q = min(qidx),
  last_q  = max(qidx),
  nq      = .N,
  r24_first = r24[which.min(qidx)],
  r24_last  = r24[which.max(qidx)],
  r13_first = if (all(is.na(r13))) NA_integer_ else r13[which.min(qidx)],
  r13_last  = if (all(is.na(r13))) NA_integer_ else r13[which.max(qidx)],
  assets_first = assets[which.min(qidx)],
  fips_first   = fips[which.min(qidx)],
  fips_last    = fips[which.max(qidx)],
  n_fips       = uniqueN(fips[!is.na(fips)])
), by = id]
spell[, `:=`(
  exited  = as.integer(last_q  < QMAX),
  entered = as.integer(first_q > QMIN)
)]
cat(sprintf("Spells: %d CUs | %d exits | %d entries | %d present throughout\n",
            nrow(spell), sum(spell$exited), sum(spell$entered),
            spell[first_q == QMIN & last_q == QMAX, .N]))

## Balanced check on gaps — a CU that disappears and returns breaks the flow
## identity in 12.3. Quantify before assuming it away.
gaps <- sp[, list(gap = (max(qidx) - min(qidx) + 1L) - .N), by = id][gap > 0]
cat(sprintf("CUs with interior gaps in the panel: %d (%.2f%%)\n",
            nrow(gaps), 100 * nrow(gaps) / nrow(spell)))

## ---- 12.2  charter counts and the indexed trend ----------------------------
## The headline Q2 number. Held at a FIXED vintage — never contemporaneous.

count_series <- function(vint = "r24") {
  x <- sp[!is.na(get(vint)), list(n = .N), by = list(qidx, rural = get(vint))]
  x <- dcast(x, qidx ~ rural, value.var = "n", fill = 0L)
  setnames(x, c("0", "1"), c("n_nonrural", "n_rural"), skip_absent = TRUE)
  x[, vintage := vint]
  x[]
}
cnt24 <- count_series("r24")
cnt13 <- if (!is.na(C$r13)) count_series("r13") else NULL

index_to_base <- function(x, base_q = QMIN) {
  b_r <- x[qidx == base_q, n_rural]; b_n <- x[qidx == base_q, n_nonrural]
  x[, `:=`(idx_rural    = 100 * n_rural    / b_r,
           idx_nonrural = 100 * n_nonrural / b_n)]
  x[]
}
cnt24 <- index_to_base(cnt24)
if (!is.null(cnt13)) cnt13 <- index_to_base(cnt13)

Q2$Q2_1_charter_counts <- rbindlist(list(cnt24, cnt13), use.names = TRUE, fill = TRUE)
Q2$Q2_1_charter_counts[, period := qlab(qidx)]

cat("\n--- 12.2  Charter counts, endpoints ---\n")
print(Q2$Q2_1_charter_counts[qidx %in% c(QMIN, QMAX),
      list(vintage, period, n_rural, n_nonrural, idx_rural, idx_nonrural)])

## Annual endpoints, the version that goes in the report table.
Q2$Q2_2_annual <- Q2$Q2_1_charter_counts[
  qidx %% 4L == QMAX %% 4L,
  list(vintage, period, n_rural, n_nonrural, idx_rural, idx_nonrural)]

## THE Q2 TEST, stated as a single comparison:
##   if idx_rural(final) > idx_nonrural(final) then rural charters have fallen
##   LESS than the system, and "rural decline" is not the honest headline.
with(cnt24[qidx == QMAX], cat(sprintf(
  "\nQ2 headline (2024 vintage): rural index %.1f vs non-rural %.1f -> rural charters fell %s\n",
  idx_rural, idx_nonrural,
  if (idx_rural > idx_nonrural) "MORE SLOWLY than the system" else "FASTER than the system")))

## ---- 12.3  flow decomposition and the accounting identity ------------------
## For each quarter t -> t+1, holding vintage fixed:
##   dN_rural = entries_rural - exits_rural + in_migration - out_migration
## Migration here is a CU that stayed alive and changed HQ county across the
## rural line. With vintage fixed there is no map-redrawing term, so the
## identity must close exactly. It is asserted below, not assumed.

flow_decomp <- function(vint = "r24") {
  w <- sp[!is.na(get(vint)), list(id, qidx, r = as.integer(get(vint)))]
  setkey(w, id, qidx)
  nxt <- copy(w)[, qidx := qidx - 1L]
  setnames(nxt, "r", "r_next")
  m <- merge(w, nxt, by = c("id", "qidx"), all = TRUE)

  m[, `:=`(
    is_exit  = as.integer(!is.na(r) & is.na(r_next)),
    is_entry = as.integer( is.na(r) & !is.na(r_next))
  )]
  m[!is.na(r) & !is.na(r_next), `:=`(
    move_in  = as.integer(r == 0L & r_next == 1L),
    move_out = as.integer(r == 1L & r_next == 0L)
  )]

  f <- m[, list(
    exit_rural      = sum(is_exit  * (r == 1L),      na.rm = TRUE),
    exit_nonrural   = sum(is_exit  * (r == 0L),      na.rm = TRUE),
    entry_rural     = sum(is_entry * (r_next == 1L), na.rm = TRUE),
    entry_nonrural  = sum(is_entry * (r_next == 0L), na.rm = TRUE),
    move_in_rural   = sum(move_in,  na.rm = TRUE),
    move_out_rural  = sum(move_out, na.rm = TRUE)
  ), by = qidx][qidx >= QMIN & qidx < QMAX]

  f[, net_rural := entry_rural - exit_rural + move_in_rural - move_out_rural]
  f[, vintage := vint]
  f[]
}
fl24 <- flow_decomp("r24")

## Identity check. If this fails, the cause is interior gaps (see 12.1) — the
## flow logic is right and the panel has re-entrants.
chk <- merge(fl24[, list(qidx, net_rural)],
             cnt24[, list(qidx, n_rural)], by = "qidx")
chk <- merge(chk, cnt24[, list(qidx = qidx - 1L, n_rural_next = n_rural)], by = "qidx")
chk[, resid := (n_rural_next - n_rural) - net_rural]
cat(sprintf("\n--- 12.3  Identity check: %d of %d quarters close exactly; max |resid| = %d\n",
            chk[resid == 0, .N], nrow(chk), max(abs(chk$resid))))
if (any(chk$resid != 0)) print(chk[resid != 0])

## Roll to annual — the form the report uses.
fl24[, year := (qidx - 1L) %/% 4L]
Q2$Q2_3_flows <- fl24[, lapply(.SD, sum), by = year,
  .SDcols = c("exit_rural","entry_rural","move_in_rural","move_out_rural",
              "exit_nonrural","entry_nonrural")]
Q2$Q2_3_flows[, net_rural := entry_rural - exit_rural + move_in_rural - move_out_rural]
cat("\n--- 12.3  Annual rural flows ---\n"); print(Q2$Q2_3_flows)

cat(sprintf("\nPanel totals (rural): %d entries, %d exits, %d moved in, %d moved out\n",
            sum(fl24$entry_rural), sum(fl24$exit_rural),
            sum(fl24$move_in_rural), sum(fl24$move_out_rural)))

## Are the entries real de novos? A genuine new charter starts tiny. An "entry"
## that arrives holding $80m is a conversion, a re-registration, or a merger
## artifact. This is the honest denominator for the formation claim.
ent <- spell[entered == 1L]
Q2$Q2_3b_entry_size <- ent[, list(
  n = .N,
  under_2m  = sum(assets_first <   2e6, na.rm = TRUE),
  under_5m  = sum(assets_first <   5e6, na.rm = TRUE),
  under_25m = sum(assets_first <  25e6, na.rm = TRUE),
  median_first_assets = as.numeric(median(assets_first, na.rm = TRUE))
), by = list(rural_at_entry = r24_first)]
cat("\n--- 12.3b  Size at first appearance — which 'entries' are plausibly de novo ---\n")
print(Q2$Q2_3b_entry_size)

## ---- 12.3c  the list of rural entrants --------------------------------------
## One row per credit union that first appears after the panel opens with its
## headquarters in a rural county (2024 vintage). This is the list behind the
## entry count. It counts each credit union once; the quarterly flow total in
## 12.3 also counts a credit union that drops out and comes back (see `gaps`).

first_row <- sp[, .SD[which.min(qidx)], by = id]
rural_entrants <- spell[entered == 1L & r24_first == 1L,
                        list(id, first_q, last_q, fips_first, assets_first, exited)]
rural_entrants <- merge(rural_entrants,
                        first_row[, list(id, st, members_first = members, nw_first = nw)],
                        by = "id", all.x = TRUE)
## name and charter type, if cr carries them
if (!is.na(C$name))  rural_entrants[cr[, list(id = get(C$id), name = get(C$name))][!duplicated(id)],
                                    on = "id", name := i.name]
if (!is.na(C$ctype)) rural_entrants[cr[, list(id = get(C$id), cu_type = get(C$ctype))][!duplicated(id)],
                                    on = "id", cu_type := i.cu_type]
rural_entrants[, `:=`(
  first_quarter  = qlab(first_q),
  status         = fifelse(exited == 0L, "still open", paste("gone after", qlab(last_q))),
  assets_first_m = round(assets_first / 1e6, 2),
  under_2m       = as.integer(assets_first < 2e6),
  under_5m       = as.integer(assets_first < 5e6),
  in_and_out     = as.integer(id %in% gaps$id)          # left the panel and came back
)]
setorder(rural_entrants, first_q)

cat(sprintf("\n--- 12.3c  Rural entrants: %d credit unions (flow total in 12.3: %d)\n",
            nrow(rural_entrants), sum(fl24$entry_rural)))
cat(sprintf("    under $2m at entry: %d | under $5m: %d | still open: %d | with panel gaps: %d\n",
            sum(rural_entrants$under_2m, na.rm = TRUE), sum(rural_entrants$under_5m, na.rm = TRUE),
            sum(rural_entrants$exited == 0L), sum(rural_entrants$in_and_out)))
if ("cu_type" %in% names(rural_entrants)) print(table(cu_type = rural_entrants$cu_type))   ## LOOK -- only 1 and 2
print(rural_entrants)                                                                      ## LOOK -- the list
Q2$Q2_3c_rural_entrants <- rural_entrants

## ---- 12.4  rates, not counts -----------------------------------------------
## Counts are confounded by base size: there are 8x more non-rural CUs. Rates
## per 100 charters at risk are the comparison that answers Q2.

rate_tab <- merge(fl24[, list(qidx, exit_rural, exit_nonrural,
                              entry_rural, entry_nonrural)],
                  cnt24[, list(qidx, n_rural, n_nonrural)], by = "qidx")
rate_tab[, `:=`(
  exit_rate_rural     = 100 * exit_rural     / n_rural,
  exit_rate_nonrural  = 100 * exit_nonrural  / n_nonrural,
  entry_rate_rural    = 100 * entry_rural    / n_rural,
  entry_rate_nonrural = 100 * entry_nonrural / n_nonrural
)]
rate_tab[, year := (qidx - 1L) %/% 4L]

Q2$Q2_4_rates <- rate_tab[, list(
  exit_rate_rural     = sum(exit_rural)    / mean(n_rural)    * 100,
  exit_rate_nonrural  = sum(exit_nonrural) / mean(n_nonrural) * 100,
  entry_rate_rural    = sum(entry_rural)   / mean(n_rural)    * 100,
  entry_rate_nonrural = sum(entry_nonrural)/ mean(n_nonrural) * 100
), by = year]
cat("\n--- 12.4  Annual exit and entry rates per 100 charters ---\n")
print(Q2$Q2_4_rates)

## Whole-panel cumulative exit share, the number quoted in the findings note.
Q2$Q2_4b_cumulative <- spell[!is.na(r24_first), list(
  n_cus       = .N,
  n_exited    = sum(exited),
  exit_share  = 100 * mean(exited),
  n_entered   = sum(entered),
  entry_share = 100 * mean(entered)
), by = list(rural = r24_first)]
print(Q2$Q2_4b_cumulative)

## ---- 12.5  survival --------------------------------------------------------
## Reproduces the 8.7 result inside the Q2 frame and adds the survival-curve
## quantities, which report better than a hazard ratio.

surv_dat <- spell[!is.na(r24_first)]
surv_dat[, `:=`(
  t_start = first_q - QMIN,
  t_stop  = last_q  - QMIN + 1L,
  event   = exited,
  rural   = r24_first
)]
## Left-truncate CUs already alive at panel start so entry timing is not
## mistaken for a birth.
km <- survfit(Surv(t_start, t_stop, event) ~ rural, data = surv_dat)
print(km)

Q2$Q2_5_survival <- {
  s <- summary(km, times = c(20L, 40L, 63L))   # 5yr, 10yr, ~15.75yr in quarters
  data.table(strata = as.character(s$strata), qtrs = s$time,
             years = s$time / 4, surv = s$surv, lower = s$lower, upper = s$upper)
}
cat("\n--- 12.5  Survival at 5 / 10 / 15 years ---\n"); print(Q2$Q2_5_survival)

cox_q2 <- coxph(Surv(t_start, t_stop, event) ~ rural, data = surv_dat)
cat("\nRaw hazard ratio (rural vs non-rural):\n")
print(summary(cox_q2)$conf.int)
cat("Conditional hazard is estimated in 8_q1.R (0.745) — do not re-report this raw figure alone.\n")

## ---- 12.6  exit composition ------------------------------------------------
## BLOCKING: outcome and reason code meanings are NOT yet verified against the
## NCUA data dictionary. Code MC carries ~80% of rural exits. "220 mergers" and
## "220 failures" are different sentences and the report paragraph rests on
## which it is. Fill OUTCOME_LOOKUP below once verified; until then every label
## prints as UNVERIFIED and nothing downstream should be quoted.

OUTCOME_LOOKUP <- data.table(
  outcome = character(),   # e.g. "MC"
  meaning = character()    # e.g. "merger, consolidation" — VERIFY FIRST
)
REASON_LOOKUP <- data.table(reason = character(), meaning = character())

if (!is.na(C$outcome)) {
  last_rec <- sp[spell[exited == 1L], on = list(id, qidx = last_q)]
  comp <- last_rec[!is.na(r24), list(n = .N), by = list(rural = r24, outcome)]
  comp[, share := 100 * n / sum(n), by = rural]
  comp <- merge(comp, OUTCOME_LOOKUP, by = "outcome", all.x = TRUE)
  comp[is.na(meaning), meaning := "UNVERIFIED — pull the data dictionary"]
  Q2$Q2_6_exit_outcome <- comp[order(rural, -n)]
  cat("\n--- 12.6  Exit composition by outcome code ---\n")
  print(Q2$Q2_6_exit_outcome)

  if (!is.na(C$reason)) {
    cmr <- last_rec[!is.na(r24), list(n = .N), by = list(rural = r24, outcome, reason)]
    cmr[, share := 100 * n / sum(n), by = rural]
    Q2$Q2_6b_exit_outcome_reason <- cmr[order(rural, -n)]
    print(head(Q2$Q2_6b_exit_outcome_reason[rural == 1L], 10))
  }
  cat("\n*** Labels are UNVERIFIED. Do not write 'merger' or 'failure' from this table. ***\n")
}

## ---- 12.7  where do exiting rural charters go? -----------------------------
## Q2's second half. If a rural charter is absorbed by a non-rural acquirer,
## the county keeps its offices (see 11_) but loses local control. That is a
## different sentence from "the credit union closed" and the report needs it.

if (!is.na(C$ptr) && !is.na(C$join)) {
  ex <- sp[spell[exited == 1L], on = list(id, qidx = last_q)][r24 == 1L]
  ex <- ex[!is.na(ptr) & ptr != "" & ptr != "0"]

  ## Successor rurality, measured in the quarter AFTER the exit.
  succ <- sp[, list(join, qidx, succ_rural = r24, succ_assets = assets)]
  ex[, look_q := qidx + 1L]
  ex2 <- merge(ex, succ, by.x = c("ptr", "look_q"), by.y = c("join", "qidx"),
               all.x = TRUE)

  Q2$Q2_7_acquirer <- ex2[, list(
    n = .N,
    to_rural      = sum(succ_rural == 1L, na.rm = TRUE),
    to_nonrural   = sum(succ_rural == 0L, na.rm = TRUE),
    unmatched     = sum(is.na(succ_rural)),
    pct_to_nonrural = 100 * sum(succ_rural == 0L, na.rm = TRUE) /
                      sum(!is.na(succ_rural))
  )]
  cat("\n--- 12.7  Destination of exiting rural charters ---\n")
  print(Q2$Q2_7_acquirer)

  ## Weight by what actually moved.
  Q2$Q2_7b_acquirer_weighted <- ex2[!is.na(succ_rural), list(
    n = .N,
    members_moved = sum(members, na.rm = TRUE),
    assets_moved  = sum(assets,  na.rm = TRUE)
  ), by = list(succ_rural)]
  print(Q2$Q2_7b_acquirer_weighted)
} else {
  message("12.7 skipped — successor pointer or join number not available in `cr`.")
}

## ---- 12.7c  the list of rural exits ------------------------------------------
## One row per credit union whose last quarter is before the panel ends and
## whose headquarters was in a rural county (2024 vintage) in that last quarter.
## This is the list behind the exit count. It counts each credit union once;
## the quarterly flow total in 12.3 also counts a temporary drop-out (see `gaps`).
## Outcome and reason codes are printed as codes: their meanings are still
## UNVERIFIED (see 12.6), so do not label any row "merger" or "failure" yet.

last_row <- sp[spell[exited == 1L, list(id, last_q)], on = list(id, qidx = last_q)]
rural_exits <- last_row[r24 == 1L, list(id, st, fips_last = fips, last_q = qidx,
                                        assets_last = assets, members_last = members,
                                        nw_last = nw, roa_last = roa)]
rural_exits <- merge(rural_exits, spell[, list(id, first_q)], by = "id", all.x = TRUE)
## name, charter type, outcome / reason codes, successor pointer -- whichever cr carries
if (!is.na(C$name))    rural_exits[cr[, list(id = get(C$id), name = get(C$name))][!duplicated(id)], on = "id", name := i.name]
if (!is.na(C$ctype))   rural_exits[cr[, list(id = get(C$id), cu_type = get(C$ctype))][!duplicated(id)], on = "id", cu_type := i.cu_type]
if (!is.na(C$outcome)) rural_exits[last_row, on = list(id, last_q = qidx), outcome := i.outcome]
if (!is.na(C$reason))  rural_exits[last_row, on = list(id, last_q = qidx), reason  := i.reason]
## successor: who it went to, looked up in the quarter after the exit
if (!is.na(C$ptr) && !is.na(C$join)) {
  rural_exits[last_row, on = list(id, last_q = qidx), successor_join := i.ptr]
  rural_exits[successor_join %in% c("", "0"), successor_join := NA]
  succ_info <- sp[, list(successor_join = join, look_q = qidx, successor_id = id, successor_state = st,
                         successor_rural = r24, successor_assets_m = round(assets / 1e6, 1))]
  rural_exits[, look_q := last_q + 1L]
  rural_exits <- merge(rural_exits, succ_info, by = c("successor_join", "look_q"), all.x = TRUE)
  if (!is.na(C$name)) rural_exits[cr[, list(successor_id = get(C$id), successor_name = get(C$name))][!duplicated(successor_id)],
                                  on = "successor_id", successor_name := i.successor_name]
  rural_exits[, look_q := NULL]
}
rural_exits[, `:=`(
  first_quarter = qlab(first_q),
  exit_quarter  = qlab(last_q),
  exit_year     = (last_q - 1L) %/% 4L,
  assets_last_m = round(assets_last / 1e6, 2),
  in_and_out    = as.integer(id %in% gaps$id)            # had a gap in the panel before leaving
)]
setorder(rural_exits, last_q)

cat(sprintf("\n--- 12.7c  Rural exits: %d credit unions (flow total in 12.3: %d)\n",
            nrow(rural_exits), sum(fl24$exit_rural)))
if ("cu_type" %in% names(rural_exits)) print(table(cu_type = rural_exits$cu_type))             ## LOOK -- only 1 and 2
print(rural_exits[, .N, by = exit_year][order(exit_year)])                                       ## LOOK -- exits per year
if ("outcome" %in% names(rural_exits)) print(rural_exits[, .N, by = outcome][order(-N)])        ## LOOK -- codes only, UNVERIFIED
if ("successor_rural" %in% names(rural_exits))
  print(rural_exits[, .N, by = list(successor = fifelse(is.na(successor_rural), "no successor found",
                                              fifelse(successor_rural == 1L, "rural", "non-rural")))])   ## LOOK
cat(sprintf("    median assets at exit: $%.1fm\n", median(rural_exits$assets_last_m, na.rm = TRUE)))
print(rural_exits)                                                                               ## LOOK -- the list
Q2$Q2_7c_rural_exits <- rural_exits

## ---- 12.8  charter-level relocation ----------------------------------------
## The site-level version (11_) found relocation negligible: 35 of 35,356 sites
## crossed the rural line. Confirm the same at charter level before the report
## claims the network changes through openings and closings, not moves.

reloc <- spell[n_fips > 1L]
reloc[, crossed := as.integer(!is.na(r24_first) & !is.na(r24_last) &
                              r24_first != r24_last)]
Q2$Q2_8_relocation <- data.table(
  cus_total          = nrow(spell),
  cus_changed_county = nrow(reloc),
  crossed_rural_line = sum(reloc$crossed, na.rm = TRUE),
  rural_to_nonrural  = reloc[r24_first == 1L & r24_last == 0L, .N],
  nonrural_to_rural  = reloc[r24_first == 0L & r24_last == 1L, .N]
)
cat("\n--- 12.8  Charter HQ relocation ---\n"); print(Q2$Q2_8_relocation)

## ---- 12.9  county coverage -------------------------------------------------
## The measure a member of Congress will actually ask about: how many rural
## counties still have a credit union headquartered in them?

cov_q <- function(q) {
  sp[qidx == q & r24 == 1L, list(n_cus = .N), by = fips]
}
rural_universe <- if (exists("uic24")) {
  u <- as.data.table(uic24)
  fcol <- pick_col(u, c("^fips", "^statecty", "^county_fips"), "uic fips")
  rcol <- pick_col(u, c("^rural", "^is_rural"), "uic rural flag", FALSE)
  if (!is.na(rcol)) unique(u[get(rcol) == 1L][[fcol]]) else unique(u[[fcol]])
} else NULL

cov_first <- cov_q(QMIN); cov_last <- cov_q(QMAX)
Q2$Q2_9_coverage <- data.table(
  period = c(qlab(QMIN), qlab(QMAX)),
  rural_counties_with_hq = c(uniqueN(cov_first$fips), uniqueN(cov_last$fips)),
  rural_counties_total   = if (!is.null(rural_universe)) length(rural_universe) else NA_integer_
)
Q2$Q2_9_coverage[, pct_covered := 100 * rural_counties_with_hq / rural_counties_total]
cat("\n--- 12.9  Rural counties with a locally headquartered CU ---\n")
print(Q2$Q2_9_coverage)

## Counties that lost their only CU — the concrete, quotable list.
lost <- setdiff(cov_first$fips, cov_last$fips)
gained <- setdiff(cov_last$fips, cov_first$fips)
cat(sprintf("Rural counties that lost all local CU HQs: %d | that gained one: %d\n",
            length(lost), length(gained)))
Q2$Q2_9b_county_change <- data.table(
  fips = c(lost, gained),
  change = c(rep("lost_last_hq", length(lost)), rep("gained_hq", length(gained)))
)

## ---- 12.10  write and summarise --------------------------------------------

for (nm in names(Q2)) {
  fwrite(Q2[[nm]], file.path(Q2_DIR, paste0(nm, ".csv")))
}
cat(sprintf("\nWrote %d Q2 exhibits to %s\n", length(Q2), Q2_DIR))

cat("\n==================== Q2 — THE ANSWER ====================\n")
with(cnt24[qidx == QMAX], cat(sprintf(
"Charters, %s = 100 -> %s: rural %.1f, non-rural %.1f.\n", qlab(QMIN), qlab(QMAX),
  idx_rural, idx_nonrural)))
cat(sprintf("Cumulative exit share: rural %.1f%%, non-rural %.1f%%.\n",
            Q2$Q2_4b_cumulative[rural == 1L, exit_share],
            Q2$Q2_4b_cumulative[rural == 0L, exit_share]))
cat(sprintf("Entries: %d rural against %d rural exits.\n",
            sum(fl24$entry_rural), sum(fl24$exit_rural)))
cat(sprintf("Relocation across the rural line: %d charters of %d.\n",
            Q2$Q2_8_relocation$crossed_rural_line, Q2$Q2_8_relocation$cus_total))
cat("\nStill open: outcome/reason code meanings (12.6) — BLOCKING for the\n",
    "exit-composition paragraph in both drafted sections.\n", sep = "")
cat("=========================================================\n")
