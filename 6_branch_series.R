###############################################################################
# 6_branch_series.R  -- CU x quarter branch series, openings/closings, S1 vs S2
#
# The payoff is section 6.5: whether the headquarters-based rural definition
# (S1) agrees with a real service-area definition (S2). If it does, every
# finding from 7_exhibits.R stands.
###############################################################################

## ---- 6.1 credit union x quarter series --------------------------------------
ts <- b[!foreign & !terr, .(
  n_sites       = .N,
  n_hq          = sum(main_office, na.rm = TRUE),
  n_branches    = sum(!main_office, na.rm = TRUE),
  n_classified  = sum(!is.na(rural_site)),
  n_rural_sites = sum(rural_site == 1L, na.rm = TRUE),
  n_urban_sites = sum(rural_site == 0L, na.rm = TRUE),
  hq_rural      = as.integer(any(main_office & rural_site == 1L, na.rm = TRUE))
), by = .(cu_number, year, q, qidx)]

ts[, rural_share := fifelse(n_classified > 0, n_rural_sites / n_classified, NA_real_)]
ts[, `:=`(S2_majority = as.integer(rural_share >= 0.5),
          S2_any      = as.integer(n_rural_sites > 0))]
setorder(ts, cu_number, qidx)

dim(ts)                                                   ## LOOK
ts[1:5]                                                   ## LOOK
ts[qidx == max(qidx), .(cus = .N, median_sites = median(n_sites),
                        rural_by_S2 = sum(S2_majority))]  ## LOOK

## national totals by quarter -- the shape of the branch network
ts[, .(sites = sum(n_sites), rural = sum(n_rural_sites),
       urban = sum(n_urban_sites)), by = .(year, q)][order(year, q)]   ## LOOK

## ---- 6.2 site lifecycle -----------------------------------------------------
## SiteId is stable across quarters, which is what makes openings and closings
## identifiable rather than just net counts.
##
## START AT 2013Q1. The branch table was introduced Sep-2010 and coverage
## filled in through 2012 -- site counts run ~19,000 in 2010 and ~22,800 by
## 2011Q1, and 2012 shows roughly three times the steady-state opening rate.
## Those are reporting artifacts, not branches being built.
START_Q <- 2013 * 4L + 1L

s <- b[!foreign & !terr & qidx >= START_Q,
       .(cu_number, site_id, qidx, year, q, fips, rural_site)]
allq <- sort(unique(s$qidx)); length(allq)                ## LOOK

life <- s[, .(first_q = min(qidx), last_q = max(qidx), n_q = .N,
              first_rural = rural_site[which.min(qidx)],
              last_rural  = rural_site[which.max(qidx)],
              first_fips  = fips[which.min(qidx)],
              last_fips   = fips[which.max(qidx)]),
          by = .(cu_number, site_id)]
life[, `:=`(opened  = first_q > min(allq),
            closed  = last_q  < max(allq),
            moved   = !is.na(first_fips) & !is.na(last_fips) & first_fips != last_fips,
            flipped = !is.na(first_rural) & !is.na(last_rural) & first_rural != last_rural)]

life[, .(sites = .N, opened = sum(opened), closed = sum(closed),
         moved_county = sum(moved), flipped_rural = sum(flipped))]     ## LOOK

## is SiteId stable, or churning? short-lived sites would mean fake events
life[, .N, by = .(quarters = pmin(n_q, 10))][order(quarters)]          ## LOOK
## a large mass at 10+ means the identifier is real

## openings by year -- if the first year is far above the rest, the window
## still starts too early
life[opened == TRUE, .N, by = .(year = first_q %/% 4L)][order(year)]   ## LOOK

## rural flips: fixed vintage, so these are genuine relocations
life[flipped == TRUE, .N,
     by = .(direction = fifelse(first_rural == 1L, "rural -> urban", "urban -> rural"))]  ## LOOK

## ---- 6.3 openings and closings by year and rural status ---------------------
op <- s[life[opened == TRUE], on = .(cu_number, site_id, qidx = first_q),
        .(openings = .N), by = .(year = qidx %/% 4L, rural = rural_site)]
cl <- s[life[closed == TRUE], on = .(cu_number, site_id, qidx = last_q),
        .(closings = .N), by = .(year = qidx %/% 4L, rural = rural_site)]
flow <- merge(op, cl, by = c("year", "rural"), all = TRUE)
setnafill(flow, fill = 0, cols = c("openings", "closings"))
flow[, net := openings - closings]

dcast(flow[!is.na(rural)], year ~ rural, value.var = c("openings", "closings", "net"))  ## LOOK

## This is the Q8 access story: whether rural counties are losing credit union
## presence, and whether closings accelerated.

## ---- 6.4 reclassification, kept separate from movement ----------------------
rec <- b[qidx == max(qidx) & !foreign & !terr &
         !is.na(rural_site) & !is.na(rural_site_13),
         .(sites = .N, cus = uniqueN(cu_number)), by = .(rural_site_13, rural_site)]
rec[, status := fifelse(rural_site_13 == rural_site, "stable",
                 fifelse(rural_site_13 == 1L, "lost rural", "gained rural"))]
rec                                                       ## LOOK

## Compare the reclassified count against life[flipped == TRUE] above. If
## reclassification is several times larger than real movement, that ratio is
## itself a report exhibit -- the definition moves more institutions than the
## institutions do.

## ---- 6.5 THE TEST: S1 (HQ county) vs S2 (majority of sites) -----------------
cr2 <- merge(cr, ts[, .(cu_number, qidx, n_sites, n_hq, n_branches,
                        n_rural_sites, rural_share, S2_majority, S2_any, hq_rural)],
             by = c("cu_number", "qidx"), all.x = TRUE)

cr2[, .(pct_with_branch = round(100 * mean(!is.na(n_sites)), 1)), by = year][order(year)]  ## LOOK
## 2010-2012 will be low (branch table still filling in); 2013+ should be ~99.8%

d <- cr2[!is.na(S2_majority) & !is.na(rural)]
d[, .N, by = .(S1 = rural, S2 = S2_majority)][order(-S1, -S2)]         ## LOOK
round(100 * d[, mean(rural == S2_majority)], 2)                        ## LOOK -- agreement

## Above ~95%: the HQ-based findings stand and S2 is a robustness row.
## Materially below: re-run the exhibits on S2 before publishing anything.

## which direction do the disagreements run?
d[rural != S2_majority,
  .(cu_quarters = .N, cus = uniqueN(cu_number)),
  by = .(case = fifelse(rural == 1L, "rural HQ, metro footprint",
                                     "metro HQ, rural footprint"))]    ## LOOK

## More "metro HQ, rural footprint" than the reverse means HQ-based
## classification UNDERSTATES rural service -- a finding, not just a caveat.

saveRDS(ts,  file.path(OUT_DIR, "cu_branch_timeseries.rds"))
fwrite(flow, file.path(OUT_DIR, "branch_openings_closings.csv"))

cat("\nBranch series ready as `ts`, merged panel as `cr2`. Next: 7_exhibits.R\n")
