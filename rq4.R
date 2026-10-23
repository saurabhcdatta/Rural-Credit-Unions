###############################################################################
# rq4.R  --  Q4: Capital or customers?
#            What limits an existing rural credit union's growth -- the capital
#            it holds, the members it can reach, or the deposits it can lend?
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# HOW TO READ THIS SCRIPT
#   Run it one block at a time (select the block, Ctrl+Enter). Every object
#   stays in your environment so you can look at it. Lines ending in
#   "## LOOK" print something you should read before moving on.
#   Style: plain dplyr, one verb per line, no shortcuts. If a line is not
#   obvious, there is a comment above it saying what it does and why.
#
# THE IDEA
#   A credit union can be held back by three different things:
#     CAPITAL  -- it cannot grow faster than its earnings rebuild its net
#                 worth ratio ("sustainable growth" = ROA / net worth ratio).
#     MEMBERS  -- it has already signed up most of the people it is allowed
#                 to serve (penetration = members / potential members), or its
#                 county is losing people.
#     FUNDING  -- it has already lent out most of its deposits (loans/shares),
#                 so it cannot make more loans without attracting more savings.
#     REACH    -- the members it could sign up are too far from its offices,
#                 or too few (county population, density, its own branch
#                 network, and how many other credit unions and banks compete
#                 for the same people).
#   The statute assumes the first. Sec. 4.5 tests all four side by side.
#   "Customers" is therefore three things: how many people are in reach
#   (REACH), how many have already joined (MEMBERS), and how contested the
#   market is (competition, inside REACH).
#
# WHAT COMES IN   cr, county, sites, offices_q (from rq_load.R, or the legacy shim)
#                 optional: banks_q (M6) and access (M7) when the master has them
# WHAT GOES OUT   tables printed to the console, rq4_*.csv in output/rq4/,
#                 and results written to the register for the slides.
# PACKAGES        dplyr, tidyr, fixest (one regression call), ggplot2
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(ggplot2)
})

## ---- 4.0  gates: the right universe, the right tables -----------------------
## universe.R is the ONE place the study's universe is defined. assert_universe()
## stops the script if cr contains anything outside charter types 1-2.
if (!exists("assert_universe")) source(file.path(if (exists("CODE_DIR")) CODE_DIR else ".", "universe.R"))
assert_universe(cr, "rq4.R")
stopifnot(exists("county"), exists("sites"), exists("offices_q"))
OUT4 <- file.path(OUT_DIR, "rq4")
dir.create(OUT4, recursive = TRUE, showWarnings = FALSE)

## A small helper used in every block: a quarter index back to a label.
q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
QZ <- max(cr$qidx)                                         # the latest quarter in the panel
cat("Latest quarter:", q_lab(QZ), "\n")

## ---- 4.1  the analysis table: one row per credit union per quarter ----------
## We keep only the columns this question needs, and only the estimation
## sample (assets >= $1m) -- ratios on very small balance sheets are noise.
d <- cr %>%
  as_tibble() %>%
  filter(est_sample == TRUE) %>%
  select(cu_number, qidx, year, state_code, fips, rural, band, lid,
         assets_tot, members, members_pot, lns_tot, dep_tot,
         networth_pct, roa_pct,
         g_assets, g_members, g_loans) %>%
  mutate(
    ## CAPITAL: the fastest a credit union can grow and keep its net worth
    ## ratio where it is, funded only by its own earnings. Both inputs are
    ## percentages, so the ratio is a percentage too.
    ##   example: ROA 0.92% and net worth 13.4%  ->  0.92 / 13.4 * 100 = 6.9% a year
    sustainable_growth = ifelse(networth_pct > 0, roa_pct / networth_pct * 100, NA_real_),
    capital_headroom   = sustainable_growth - g_assets,     # positive = room to grow faster
    ## MEMBERS: share of the people it is allowed to serve who have joined.
    ## members_pot is self-reported and sometimes missing or tiny; we treat
    ## anything under 100 potential members as unusable.
    penetration = ifelse(!is.na(members_pot) & members_pot >= 100, members / members_pot, NA_real_),
    ## FUNDING: loans as a share of deposits. Near or above 100% means it
    ## cannot lend more without more savings coming in.
    loan_to_share = ifelse(!is.na(dep_tot) & dep_tot > 0, lns_tot / dep_tot, NA_real_)
  )

## Penetration above 1 means more members than reported potential members --
## usually an under-reported field, not a real number. Cap at 1.5 and flag it.
d <- d %>%
  mutate(penetration_flagged = !is.na(penetration) & penetration > 1,
         penetration = pmin(penetration, 1.5))

d %>%
  summarise(rows = n(), credit_unions = n_distinct(cu_number),
            pct_penetration_usable  = round(100 * mean(!is.na(penetration)), 1),
            pct_penetration_flagged = round(100 * mean(penetration_flagged), 1)) %>%
  print()                                                                        ## LOOK -- how much of the field is usable

## ---- 4.2  CAPITAL: sustainable growth and headroom ----------------------------
## This reproduces the July finding on the master tables. Medians, latest
## four quarters, rural vs non-rural.
cap <- d %>%
  filter(qidx > QZ - 4) %>%
  group_by(rural) %>%
  summarise(credit_unions      = n_distinct(cu_number),
            actual_growth      = median(g_assets, na.rm = TRUE),
            sustainable_growth = median(sustainable_growth, na.rm = TRUE),
            headroom           = median(capital_headroom, na.rm = TRUE),
            pct_above_sustainable = 100 * mean(g_assets > sustainable_growth, na.rm = TRUE),
            pct_under_8_networth  = 100 * mean(networth_pct < 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(cap)                                                                       ## LOOK -- expect headroom ~ +2.7 both segments

## The same, split by low-income designation (LID), because LID credit unions
## can raise secondary capital and the statute's capital remedies mostly run
## through them.
cap_lid <- d %>%
  filter(qidx > QZ - 4) %>%
  group_by(rural, lid) %>%
  summarise(credit_unions = n_distinct(cu_number),
            headroom = median(capital_headroom, na.rm = TRUE),
            pct_above_sustainable = 100 * mean(g_assets > sustainable_growth, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(cap_lid)                                                                   ## LOOK

## ---- 4.3  MEMBERS: penetration and the demand ceiling -------------------------
## (a) How saturated are rural fields of membership compared with others?
pen <- d %>%
  filter(qidx > QZ - 4, !is.na(penetration)) %>%
  group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            penetration_median = median(penetration),
            penetration_p75    = quantile(penetration, 0.75),
            pct_over_50        = 100 * mean(penetration > 0.50),
            pct_over_80        = 100 * mean(penetration > 0.80),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(pen)                                                                       ## LOOK

## (b) Does high penetration predict slower membership growth? If a credit
## union has signed up most of its field, it should be adding members more
## slowly. We compare growth across penetration quartiles, within segment.
pen_growth <- d %>%
  filter(!is.na(penetration), !is.na(g_members)) %>%
  group_by(rural) %>%
  mutate(penetration_quartile = ntile(penetration, 4)) %>%
  group_by(rural, penetration_quartile) %>%
  summarise(n = n(),
            median_member_growth = median(g_members, na.rm = TRUE),
            median_asset_growth  = median(g_assets,  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(pen_growth)                                                                ## LOOK -- growth should FALL from quartile 1 to 4 if demand binds

## (c) The county as the market: members per resident of the headquarters
## county. Cleaner than members_pot because the denominator is the Census,
## not the credit union's own form. Members can live outside the county, so
## values above 1 occur for institutions with a wide field; read it as a
## relative measure.
## The master's county table carries pop2010; the legacy shim's carries only
## pop2020 (from ERS) and pop2024. Use the earliest baseline available and say
## which one the change is measured from.
POP_BASE <- intersect(c("pop2010", "pop2016", "pop2019", "pop2020"), names(county))[1]
stopifnot(!is.na(POP_BASE), "pop2024" %in% names(county))
cat("County population change measured from", POP_BASE, "to pop2024\n")             ## LOOK
county_pop <- county %>%
  as_tibble() %>%
  select(fips, pop2024, pop_base = all_of(POP_BASE)) %>%
  mutate(county_pop_change_pct = 100 * (pop2024 / pop_base - 1))

d <- d %>%
  left_join(county_pop, by = "fips") %>%
  mutate(members_per_resident = ifelse(!is.na(pop2024) & pop2024 > 0, members / pop2024, NA_real_))

mkt <- d %>%
  filter(qidx == QZ) %>%
  group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            median_county_pop = median(pop2024, na.rm = TRUE),
            median_county_pop_change_pct = median(county_pop_change_pct, na.rm = TRUE),
            pct_in_shrinking_county = 100 * mean(county_pop_change_pct < 0, na.rm = TRUE),
            median_members_per_resident = median(members_per_resident, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(mkt)                                                                       ## LOOK -- how many rural CUs sit in counties losing people

## (d) REACH: the market in people and the network that reaches them.
## Four pieces, all at the credit union-quarter level:
##   county population and density  -- how many people are there to serve
##   the credit union's own offices  -- how many, and in how many counties
##   competitors                     -- other credit unions in the HQ county
##                                      (from cr) and bank branches (if banks_q exists)
##   distance                        -- share of HQ-county residents more than
##                                      10 miles from ANY credit union office
##                                      (only when the master's access table exists)
## Land area: the master's county table has sqmi; the legacy shim's does not,
## so fetch it once from the Census boundary file (ALAND is square metres).
if (!"sqmi" %in% names(county)) {
  suppressPackageStartupMessages(library(tigris))
  options(tigris_use_cache = TRUE)
  area <- counties(cb = TRUE, resolution = "20m", year = 2023) %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    transmute(fips = GEOID, sqmi = as.numeric(ALAND) / 2589988.11)
  area <- area %>%
    mutate(fips = ifelse(fips %in% c("02063", "02066"), "02261", fips)) %>%      # Alaska fold, as everywhere else
    group_by(fips) %>%
    summarise(sqmi = sum(sqmi), .groups = "drop")
  county <- county %>% left_join(area, by = "fips")
}
county_size <- county %>%
  as_tibble() %>%
  select(fips, sqmi) %>%
  mutate(sqmi = ifelse(is.na(sqmi) | sqmi <= 0, NA_real_, sqmi))

## the credit union's own network, per quarter: offices and distinct counties served
network <- sites %>%
  as_tibble() %>%
  filter(in_cr == TRUE, foreign == FALSE, terr == FALSE, !is.na(fips)) %>%
  group_by(cu_number, qidx) %>%
  summarise(own_offices = n(),
            counties_served = n_distinct(fips),
            .groups = "drop")

## other credit unions headquartered in the same county, same quarter (cr already carries it)
## The master's M2 builds n_cus_county; the legacy 3_callreport.R does not,
## so count it here when it is missing: credit unions in the same county and
## quarter, minus the credit union itself.
if (!"n_cus_county" %in% names(cr)) {
  cat("n_cus_county not in cr (legacy path) -- computing it here\n")
  competitors <- cr %>%
    as_tibble() %>%
    select(cu_number, qidx, fips) %>%
    group_by(fips, qidx) %>%
    mutate(n_cus_county = n() - 1L) %>%
    ungroup() %>%
    select(cu_number, qidx, n_cus_county)
} else {
  competitors <- cr %>%
    as_tibble() %>%
    select(cu_number, qidx, n_cus_county)
}

d <- d %>%
  left_join(county_size, by = "fips") %>%
  left_join(network,     by = c("cu_number", "qidx")) %>%
  left_join(competitors, by = c("cu_number", "qidx")) %>%
  mutate(density = pop2024 / sqmi,
         own_offices = replace_na(own_offices, 1L),           # a CU with no site record still has its headquarters
         counties_served = replace_na(counties_served, 1L),
         residents_per_own_office = ifelse(own_offices > 0, pop2024 / own_offices, NA_real_))

## bank branches in the HQ county, if the FDIC layer has been built
if (exists("banks_q")) {
  bank_now <- banks_q %>%
    as_tibble() %>%
    filter(qidx == max(qidx)) %>%
    select(fips, bank_branches = branches)
  d <- d %>% left_join(bank_now, by = "fips") %>% mutate(bank_branches = replace_na(bank_branches, 0L))
}
## share of HQ-county residents beyond 10 miles from any credit union office, if M7 has been built
if (exists("access")) {
  far <- access %>%
    as_tibble() %>%
    filter(period == max(period)) %>%
    select(fips, share_cu_over10)
  d <- d %>% left_join(far, by = "fips")
}

reach <- d %>%
  filter(qidx == QZ) %>%
  group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            median_county_pop   = median(pop2024, na.rm = TRUE),
            median_density      = median(density, na.rm = TRUE),
            median_own_offices  = median(as.numeric(own_offices)),
            pct_single_office   = 100 * mean(own_offices == 1L),
            median_counties_served = median(as.numeric(counties_served)),
            median_residents_per_own_office = median(residents_per_own_office, na.rm = TRUE),
            median_other_cus_in_county = median(as.numeric(n_cus_county), na.rm = TRUE),
            pct_sole_cu_in_county = 100 * mean(n_cus_county == 0L, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(reach)                                                                     ## LOOK -- how big is the market, how far does the network reach, how contested is it
if ("bank_branches" %in% names(d)) d %>% filter(qidx == QZ) %>% group_by(rural) %>%
  summarise(median_bank_branches_in_county = median(bank_branches), pct_no_bank = 100 * mean(bank_branches == 0)) %>% print()   ## LOOK
if ("share_cu_over10" %in% names(d)) d %>% filter(qidx == QZ) %>% group_by(rural) %>%
  summarise(median_share_residents_beyond_10mi = round(median(share_cu_over10, na.rm = TRUE), 3)) %>% print()                   ## LOOK

## Does a bigger network buy growth? Member growth by number of offices, rural only.
d %>%
  filter(rural == 1, qidx > QZ - 4, !is.na(g_members)) %>%
  mutate(office_band = cut(own_offices, c(0, 1, 2, 4, Inf), labels = c("1 office", "2", "3-4", "5+"))) %>%
  group_by(office_band) %>%
  summarise(credit_unions = n_distinct(cu_number),
            median_member_growth = round(median(g_members), 2),
            median_asset_growth  = round(median(g_assets, na.rm = TRUE), 2),
            median_assets_m      = round(median(assets_tot) / 1e6, 1),
            .groups = "drop") %>%
  print()                                                                        ## LOOK -- size confounds this; the regression below separates them

## ---- 4.4  FUNDING: loan-to-share ----------------------------------------------
fund <- d %>%
  filter(qidx > QZ - 4, !is.na(loan_to_share)) %>%
  group_by(rural) %>%
  summarise(credit_unions = n_distinct(cu_number),
            loan_to_share_median = median(loan_to_share),
            pct_over_90 = 100 * mean(loan_to_share > 0.90),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(fund)                                                                      ## LOOK

## ---- 4.5  WHICH ONE BINDS? one flag per constraint, one row per credit union --
## Thresholds are stated here, once, so a reviewer can argue with them:
##   capital-constrained : net worth under 8%  OR growing faster than sustainable
##   member-constrained  : penetration above 0.5 OR county lost population since 2010
##   funding-constrained : loans above 90% of deposits
##   reach-constrained   : one office only AND county density below the rural
##                         median (or, when the access table exists, more than
##                         half of county residents beyond 10 miles from any office)
## A credit union can be constrained by more than one, or by none.
rural_median_density <- d %>% filter(qidx == QZ, rural == 1) %>% summarise(m = median(density, na.rm = TRUE)) %>% pull(m)
latest <- d %>%
  filter(qidx == QZ) %>%
  mutate(
    capital_bind = (networth_pct < 8) | (g_assets > sustainable_growth),
    member_bind  = (!is.na(penetration) & penetration > 0.5) | (!is.na(county_pop_change_pct) & county_pop_change_pct < 0),
    funding_bind = !is.na(loan_to_share) & loan_to_share > 0.90,
    reach_bind   = if ("share_cu_over10" %in% names(d)) (own_offices == 1L & !is.na(share_cu_over10) & share_cu_over10 > 0.5)
                   else (own_offices == 1L & !is.na(density) & density < rural_median_density)
  ) %>%
  mutate(across(c(capital_bind, member_bind, funding_bind, reach_bind), ~ replace_na(.x, FALSE)))

binds <- latest %>%
  group_by(rural) %>%
  summarise(credit_unions = n(),
            pct_capital = 100 * mean(capital_bind),
            pct_members = 100 * mean(member_bind),
            pct_funding = 100 * mean(funding_bind),
            pct_reach   = 100 * mean(reach_bind),
            pct_none    = 100 * mean(!capital_bind & !member_bind & !funding_bind & !reach_bind),
            pct_customers_any = 100 * mean(member_bind | reach_bind),            # the demand side, either form
            pct_capital_only  = 100 * mean(capital_bind & !member_bind & !funding_bind & !reach_bind),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
print(binds)                                                                     ## LOOK -- THE SLIDE TABLE. Which column is biggest for rural?

## Sensitivity: the same table with the thresholds moved. If the ranking of
## the three constraints changes, say so in the report.
binds_sens <- latest %>%
  mutate(capital_bind = (networth_pct < 9) | (g_assets > sustainable_growth - 1),
         member_bind  = (!is.na(penetration) & penetration > 0.4) | (!is.na(county_pop_change_pct) & county_pop_change_pct < -2),
         funding_bind = !is.na(loan_to_share) & loan_to_share > 0.85,
         reach_bind   = own_offices <= 2L & !is.na(density) & density < rural_median_density * 1.5) %>%
  mutate(across(c(capital_bind, member_bind, funding_bind, reach_bind), ~ replace_na(.x, FALSE))) %>%
  group_by(rural) %>%
  summarise(pct_capital = 100 * mean(capital_bind), pct_members = 100 * mean(member_bind),
            pct_funding = 100 * mean(funding_bind), pct_reach = 100 * mean(reach_bind), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
print(binds_sens)                                                                ## LOOK -- same ranking?

## ---- 4.6  LIKE-FOR-LIKE: which constraint predicts growth? --------------------
## One regression. Asset growth on the three constraint measures, comparing
## credit unions in the same state and quarter (fixed effects), with the
## rural interaction so we can see whether the constraints work differently
## for rural institutions. Standard errors clustered by credit union.
## IMPORTANT: capital_headroom = sustainable_growth - g_assets, so it contains
## the outcome. Putting it on the right-hand side of a growth regression gives
## a large negative coefficient by construction (an identity, not an effect).
## The regression therefore uses capital capacity measured a YEAR EARLIER:
## last year's sustainable growth rate and last year's net worth ratio.
lagged <- d %>%
  transmute(cu_number, qidx = qidx + 4L,                    # shift forward so year t-1 lines up with year t
            sustainable_growth_lag = sustainable_growth,
            networth_lag = networth_pct)
d <- d %>% left_join(lagged, by = c("cu_number", "qidx"))

reg_data <- d %>%
  filter(!is.na(g_assets), !is.na(sustainable_growth_lag), !is.na(networth_lag), !is.na(penetration), !is.na(loan_to_share),
         !is.na(pop2024), !is.na(density)) %>%
  mutate(state_quarter = paste(state_code, qidx),
         ln_assets  = log(assets_tot),
         ln_pop     = log(pop2024),
         ln_density = log(density),
         ln_offices = log(own_offices),
         ln_competitors = log1p(n_cus_county))

m_growth <- feols(
  g_assets ~ sustainable_growth_lag + networth_lag + penetration + loan_to_share   # capital (lagged), members, funding
             + county_pop_change_pct + ln_pop + ln_density + ln_offices + ln_competitors   # reach
             + ln_assets
             + rural:sustainable_growth_lag + rural:networth_lag + rural:penetration + rural:loan_to_share
             + rural:county_pop_change_pct + rural:ln_pop + rural:ln_density + rural:ln_offices + rural:ln_competitors
             | state_quarter,
  data = reg_data, cluster = ~cu_number)
print(summary(m_growth))                                                         ## LOOK
## How to read it: a NEGATIVE coefficient on penetration means more-saturated
## credit unions grow more slowly (demand binds). A POSITIVE coefficient on
## sustainable_growth_lag means earnings capacity last year predicts growth this year (capital binds).
## A POSITIVE coefficient on ln_offices means a wider network buys growth
## even at the same size; a NEGATIVE one on ln_competitors means contested
## markets grow slower (this is rq1's mechanism result, seen from Q4's side).
## The rural: terms say whether the effect is stronger or weaker for rural.

## Standardised version so the three can be compared on one scale: how much
## does one standard deviation of each constraint move growth?
sd_table <- reg_data %>%
  summarise(sustainable_growth_lag = sd(sustainable_growth_lag), networth_lag = sd(networth_lag), penetration = sd(penetration),
            loan_to_share = sd(loan_to_share), county_pop_change_pct = sd(county_pop_change_pct, na.rm = TRUE),
            ln_pop = sd(ln_pop), ln_density = sd(ln_density), ln_offices = sd(ln_offices), ln_competitors = sd(ln_competitors)) %>%
  pivot_longer(everything(), names_to = "term", values_to = "sd")
effects <- tibble(term = names(coef(m_growth)), coef = coef(m_growth)) %>%
  filter(term %in% sd_table$term) %>%
  left_join(sd_table, by = "term") %>%
  mutate(growth_pp_per_sd = round(coef * sd, 3)) %>%
  arrange(desc(abs(growth_pp_per_sd)))
print(effects)                                                                   ## LOOK -- the biggest row is the constraint that matters most

## ---- 4.7  one picture: penetration against member growth, rural only ----------
p <- d %>%
  filter(rural == 1, !is.na(penetration), !is.na(g_members), qidx > QZ - 4) %>%
  ggplot(aes(x = penetration, y = g_members)) +
  geom_point(alpha = 0.25, size = 0.8, colour = "#1F6F78") +
  geom_smooth(method = "loess", se = TRUE, colour = "#C8553D") +
  coord_cartesian(ylim = c(-15, 25)) +
  labs(title = "Rural credit unions: the more of their field they have signed up, the slower they add members",
       x = "Penetration (members / potential members)", y = "Membership growth, trailing four quarters, %",
       caption = "Estimation sample (assets >= $1m), latest four quarters. Penetration capped at 1.5. Preliminary.") +
  theme_minimal(base_size = 11)
ggsave(file.path(OUT4, "rq4_penetration_vs_member_growth.png"), p, width = 10, height = 6, dpi = 200, bg = "white")

## ---- 4.8  save tables, register the numbers the slides will quote ------------
write.csv(cap,    file.path(OUT4, "rq4_capital_headroom.csv"),    row.names = FALSE)
write.csv(pen,    file.path(OUT4, "rq4_penetration.csv"),         row.names = FALSE)
write.csv(mkt,    file.path(OUT4, "rq4_county_market.csv"),       row.names = FALSE)
write.csv(reach,  file.path(OUT4, "rq4_reach_and_competition.csv"), row.names = FALSE)
write.csv(binds,  file.path(OUT4, "rq4_which_constraint_binds.csv"), row.names = FALSE)
write.csv(effects, file.path(OUT4, "rq4_standardised_effects.csv"), row.names = FALSE)

r <- function(x) round(x, 2)
if (exists("register_result")) {
  register_result(4, "headroom_rural",      r(cap$headroom[cap$rural == 1]),      "median sustainable minus actual asset growth, pp, latest 4Q")
  register_result(4, "headroom_nonrural",   r(cap$headroom[cap$rural == 0]),      "")
  register_result(4, "pct_above_sustainable_rural", r(cap$pct_above_sustainable[cap$rural == 1]), "")
  register_result(4, "penetration_median_rural",    r(pen$penetration_median[pen$rural == 1]), "members / potential members")
  register_result(4, "penetration_median_nonrural", r(pen$penetration_median[pen$rural == 0]), "")
  register_result(4, "pct_rural_in_shrinking_county", r(mkt$pct_in_shrinking_county[mkt$rural == 1]), sprintf("HQ county lost population %s-2024", sub("pop", "", POP_BASE)))
  register_result(4, "pct_bind_capital_rural", binds$pct_capital[binds$rural == 1], "net worth < 8% or growth above sustainable")
  register_result(4, "pct_bind_members_rural", binds$pct_members[binds$rural == 1], "penetration > 0.5 or shrinking county")
  register_result(4, "pct_bind_funding_rural", binds$pct_funding[binds$rural == 1], "loans > 90% of deposits")
  register_result(4, "pct_bind_reach_rural",   binds$pct_reach[binds$rural == 1], "single office in a thin county, or >50% of residents beyond 10 miles")
  register_result(4, "pct_bind_customers_any_rural", binds$pct_customers_any[binds$rural == 1], "members or reach")
  register_result(4, "pct_bind_none_rural",    binds$pct_none[binds$rural == 1], "")
  register_result(4, "pct_single_office_rural", reach$pct_single_office[reach$rural == 1], "")
  register_result(4, "pct_sole_cu_in_county_rural", reach$pct_sole_cu_in_county[reach$rural == 1], "no other CU headquartered in the county")
  register_result(4, "largest_standardised_effect", effects$term[1], sprintf("%.3f pp of growth per SD", effects$growth_pp_per_sd[1]))
}

cat("\n=== SLIDE NUMBERS, Q4 ===\n")
cat(sprintf("Capital: the typical rural credit union could grow %.1f points a year faster on its own earnings (non-rural %.1f). Only %.0f%% grow faster than their earnings allow.\n",
            cap$headroom[cap$rural == 1], cap$headroom[cap$rural == 0], cap$pct_above_sustainable[cap$rural == 1]))
cat(sprintf("Members: median penetration %.0f%% rural vs %.0f%% non-rural; %.0f%% of rural credit unions are headquartered in a county that has lost population since %s.\n",
            100 * pen$penetration_median[pen$rural == 1], 100 * pen$penetration_median[pen$rural == 0], mkt$pct_in_shrinking_county[mkt$rural == 1], sub("pop", "", POP_BASE)))
cat(sprintf("Reach: %.0f%% of rural credit unions have a single office; %.0f%% are the only credit union headquartered in their county; median county %s residents at %.0f per square mile.\n",
            reach$pct_single_office[reach$rural == 1], reach$pct_sole_cu_in_county[reach$rural == 1],
            format(reach$median_county_pop[reach$rural == 1], big.mark = ","), reach$median_density[reach$rural == 1]))
cat(sprintf("Which binds (rural): capital %.0f%%, members %.0f%%, reach %.0f%%, funding %.0f%%; customers in either form %.0f%%; none %.0f%%.\n",
            binds$pct_capital[binds$rural == 1], binds$pct_members[binds$rural == 1], binds$pct_reach[binds$rural == 1],
            binds$pct_funding[binds$rural == 1], binds$pct_customers_any[binds$rural == 1], binds$pct_none[binds$rural == 1]))
cat(sprintf("Like-for-like, the constraint that moves growth most is %s (%.2f pp per standard deviation).\n", effects$term[1], effects$growth_pp_per_sd[1]))
