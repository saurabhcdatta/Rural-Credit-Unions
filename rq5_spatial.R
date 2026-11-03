###############################################################################
# rq5_spatial.R  --  RQ5, spatial extensions
# "Where along the scale of remoteness does the rural advantage appear,
#  and does it survive neighbouring counties being alike?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# WHAT THIS ADDS TO rq5.R
#   rq5.R tested six yes/no definitions. Every one of them is a cut-off on the
#   same underlying scale of remoteness. This script estimates the advantage
#   ALONG that scale instead of at one cut-off:
#     S2  by USDA-ERS Urban Influence Code (the nine 2024 codes), each against
#         large-metro credit unions -- a step-by-step gradient
#     S3  by straight-line distance from the county to the nearest metro area,
#         in bands -- a continuous gradient that ignores the codes altogether
#     S4  the RQ1 headline re-estimated with Conley standard errors, which
#         allow credit unions in nearby counties to share shocks (50/100/200 km)
#
# EXPECTATIONS, WRITTEN DOWN BEFORE RUNNING
#   S2/S3: the advantage rises with remoteness; it is near zero for non-rural
#          non-metro counties (UIC 4-5) and clearly positive for remote ones.
#          If it switches on at the statute's line, the line is well placed;
#          if it rises smoothly, any line is partly arbitrary.
#   S4:    ranges widen but growth and ROA stay above zero.
#
# REQUIRES  cr, county, RURAL_2024, CODE_DIR (setup + rq_load / shim).
#           Packages: dplyr, tidyr, fixest, sf, tigris, ggplot2, data.table.
# NOTE      Credit unions are placed at their headquarters county's centre, so
#           distances are county-level approximations -- fine for a
#           county-based definition.
#
# Sequential blocks. Run each, read what it prints (## LOOK), then move on.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(fixest); library(sf); library(tigris); library(ggplot2)
})
options(tigris_use_cache = TRUE)
sf_use_s2(TRUE)

## ---- S0  gates -----------------------------------------------------------------
if (!exists("assert_universe")) source(file.path(CODE_DIR, "universe.R"))
assert_universe(cr, "rq5_spatial.R")
stopifnot(exists("county"), "RURAL_2024 not defined -- run setup" = exists("RURAL_2024"))
OUT5S <- file.path(OUT_DIR, "rq5")
dir.create(OUT5S, recursive = TRUE, showWarnings = FALSE)
if (!exists("q_lab")) q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
QZ <- max(cr$qidx)
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"     # as RQ1

## ---- S1  county geography: centre point and distance to the nearest metro ----------
## Shapes from 2023 (current counties, incl. Connecticut's planning regions) plus
## 2020 (Connecticut's old counties, which older call reports still carry).
c23 <- counties(cb = TRUE, resolution = "20m", year = 2023) %>% transmute(fips = GEOID)
c20 <- counties(cb = TRUE, resolution = "20m", year = 2020) %>% transmute(fips = GEOID) %>%
  filter(!fips %in% c23$fips)
shapes <- rbind(c23, c20) %>% st_transform(5070)                                 # equal-area metres
## Alaska: the panel folds Chugach + Copper River (02063, 02066) back into 02261
ak <- shapes %>% filter(fips %in% c("02063", "02066")) %>% summarise(geometry = st_union(geometry)) %>% mutate(fips = "02261")
shapes <- rbind(shapes, ak[, c("fips", "geometry")])
shapes <- shapes[as.integer(substr(shapes$fips, 1, 2)) <= 56, ]                  # states + DC (territories have no metro neighbours to measure)

## the nearest metro area: union of every metro county (UIC 1-2), measured from
## each county's centre to the metro EDGE. Metro counties themselves = 0.
metro_fips <- county %>% as_tibble() %>% filter(uic24 %in% c(1, 2)) %>% pull(fips)
metro_area <- shapes %>% filter(fips %in% metro_fips) %>% st_union()
centres    <- st_point_on_surface(shapes)                                        # inside the county, even for odd shapes
geo <- tibble(fips = shapes$fips,
              miles_to_metro = as.numeric(st_distance(centres, metro_area)) / 1609.344)
ll <- st_coordinates(st_transform(centres, 4326))
geo <- geo %>% mutate(lon = ll[, 1], lat = ll[, 2])
geo %>% left_join(county %>% as_tibble() %>% select(fips, uic24, rural24), by = "fips") %>%
  group_by(uic24) %>%
  summarise(counties = n(), median_miles = round(median(miles_to_metro), 1), .groups = "drop") %>%
  print()                                                                         ## LOOK -- miles should rise with the code (metro = 0)

## ---- S2  the estimation sample, exactly as RQ1 ---------------------------------------
est <- cr %>%
  as_tibble() %>%
  filter(est_sample == TRUE, !is.na(rural)) %>%
  select(cu_number, qidx, fips, state_code, rural, g_assets, roa_pct, networth_pct,
         ln_assets, age_q, lid, mdi, fed, cecl_flag) %>%
  left_join(county %>% as_tibble() %>% select(fips, uic24, adj_class), by = "fips") %>%
  left_join(geo, by = "fips") %>%      # no rows dropped here, so the baseline matches RQ1 exactly
  mutate(st = factor(state_code), qtr = factor(qidx))
cat(sprintf("Estimation sample: %s CU-quarters, %s CUs; %.1f%% with a location\n",
            format(nrow(est), big.mark = ","), format(n_distinct(est$cu_number), big.mark = ","),
            100 * mean(!is.na(est$lat))))                                         ## LOOK -- location should be ~100%

## the baseline: must reproduce RQ1 (growth ~ +0.99, ROA ~ +0.17)
m_base_g <- feols(as.formula(paste("g_assets ~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
m_base_r <- feols(as.formula(paste("roa_pct  ~ rural +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
cat(sprintf("Baseline: growth %+.3f, ROA %+.3f\n", coef(m_base_g)["rural"], coef(m_base_r)["rural"]))   ## LOOK

## ---- S3  GRADIENT 1: by Urban Influence Code ----------------------------------------
## One coefficient per code, each against large-metro credit unions (code 1).
## The label says what each code is in this study: metro, non-metro but not rural
## under the statute, rural next to a metro, or remote -- taken from the data.
uic_class <- county %>% as_tibble() %>% distinct(uic24, rural24, adj_class) %>% filter(!is.na(uic24)) %>%
  group_by(uic24) %>% summarise(rural24 = max(rural24), adj_class = first(na.omit(adj_class)), .groups = "drop") %>%
  mutate(class = case_when(uic24 %in% c(1, 2)                          ~ "metro",
                           rural24 == 0                                ~ "non-metro, not rural",
                           adj_class %in% c("remote micropolitan", "remote") ~ "rural, remote",
                           TRUE                                        ~ "rural, next to a metro"))
print(uic_class)                                                                  ## LOOK -- confirm which codes are which

## (rows without a 2024 code -- Connecticut's pre-2022 counties, territories -- drop from this model only)
est <- est %>% mutate(uic_f = factor(uic24))
grad_uic <- list()
for (y in c("g_assets", "roa_pct")) {
  m <- feols(as.formula(paste(y, "~ i(uic_f, ref = '1') +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct <- ct[grepl("^uic_f::", ct$term), ]
  grad_uic[[y]] <- tibble(outcome = ifelse(y == "g_assets", "Asset growth", "Return on assets"),
                          uic24 = as.integer(sub("uic_f::", "", ct$term)),
                          beta = ct$Estimate, lo = ct$Estimate - 1.96 * ct$`Std. Error`, hi = ct$Estimate + 1.96 * ct$`Std. Error`)
}
grad_uic <- bind_rows(grad_uic) %>%
  bind_rows(tibble(outcome = c("Asset growth", "Return on assets"), uic24 = 1L, beta = 0, lo = 0, hi = 0)) %>%
  left_join(uic_class %>% select(uic24, class), by = "uic24") %>%
  left_join(est %>% filter(qidx == QZ) %>% count(uic24, name = "cus_latest"), by = "uic24") %>%
  arrange(outcome, uic24)
print(grad_uic %>% mutate(across(c(beta, lo, hi), ~ round(.x, 3))), n = Inf)    ## LOOK -- does it rise with remoteness? where does it switch on?

## ---- S4  GRADIENT 2: by distance to the nearest metro area ----------------------------
## Same model, the codes replaced by distance bands. Band "metro" is the reference.
BANDS <- c(-Inf, 0.001, 15, 30, 50, 75, 100, Inf)
BLAB  <- c("metro", "0-15 mi", "15-30 mi", "30-50 mi", "50-75 mi", "75-100 mi", "100+ mi")
est <- est %>% mutate(dband = factor(cut(miles_to_metro, BANDS, labels = BLAB), levels = BLAB))
est %>% filter(qidx == QZ) %>% group_by(dband) %>%
  summarise(credit_unions = n(), pct_rural_statute = round(100 * mean(rural == 1L), 0), .groups = "drop") %>%
  print()                                                                         ## LOOK -- where the statute's line falls on the distance scale

grad_dist <- list()
for (y in c("g_assets", "roa_pct")) {
  m <- feols(as.formula(paste(y, "~ i(dband, ref = 'metro') +", CTRL, "| st^qtr")), est, cluster = ~ cu_number + fips)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct <- ct[grepl("^dband::", ct$term), ]
  grad_dist[[y]] <- tibble(outcome = ifelse(y == "g_assets", "Asset growth", "Return on assets"),
                           dband = sub("dband::", "", ct$term),
                           beta = ct$Estimate, lo = ct$Estimate - 1.96 * ct$`Std. Error`, hi = ct$Estimate + 1.96 * ct$`Std. Error`)
}
grad_dist <- bind_rows(grad_dist) %>%
  bind_rows(tibble(outcome = c("Asset growth", "Return on assets"), dband = "metro", beta = 0, lo = 0, hi = 0)) %>%
  mutate(dband = factor(dband, levels = BLAB)) %>% arrange(outcome, dband)
print(grad_dist %>% mutate(across(c(beta, lo, hi), ~ round(.x, 3))), n = Inf)   ## LOOK -- smooth rise, or a step?

## a single slope, for one sentence: points of advantage per 10 miles from a metro, outside metros only
slope <- feols(as.formula(paste("g_assets ~ I(pmin(miles_to_metro, 150) / 10) +", CTRL, "| st^qtr")),
               est %>% filter(miles_to_metro > 0), cluster = ~ cu_number + fips)
print(coeftable(slope)[1, , drop = FALSE])                                       ## LOOK -- growth per 10 miles (capped at 150)

## ---- S5  Conley standard errors: are neighbouring counties hiding a false precision? ---
## Conley SEs let credit unions within a set distance share shocks. They are
## heavy to compute, so this uses one quarter per year (Q4) -- the point is to
## compare the WIDTH of the range under each assumption on the same rows.
q4 <- est %>% filter(qidx %% 4L == 0L, !is.na(lat))
cat("Conley sample (Q4 only):", format(nrow(q4), big.mark = ","), "rows\n")
conley_tab <- list()
for (y in c("g_assets", "roa_pct", "networth_pct")) {
  f <- as.formula(paste(y, "~ rural +", CTRL, "| st^qtr"))
  m <- feols(f, q4, cluster = ~ cu_number + fips)
  se <- c(cluster_cu_county = se(m)["rural"],
          conley_50km  = se(summary(m, vcov = conley(50)))["rural"],
          conley_100km = se(summary(m, vcov = conley(100)))["rural"],
          conley_200km = se(summary(m, vcov = conley(200)))["rural"])
  conley_tab[[y]] <- tibble(outcome = y, beta = unname(coef(m)["rural"]), se_type = sub("\\.rural$", "", names(se)), se = unname(se)) %>%
    mutate(lo = beta - 1.96 * se, hi = beta + 1.96 * se, still_above_zero = lo > 0)
  cat("done:", y, "\n")
}
conley_tab <- bind_rows(conley_tab)
print(conley_tab %>% mutate(across(c(beta, se, lo, hi), ~ round(.x, 3))), n = Inf)   ## LOOK -- does any range now cross zero?

## ---- S6  charts --------------------------------------------------------------------
CLASS_COLS <- c("metro" = "#8C8C8C", "non-metro, not rural" = "#D9C9A3",
                "rural, next to a metro" = "#B08D57", "rural, remote" = "#5C4326")   # the map 15 shades
p_uic <- grad_uic %>%
  mutate(class = factor(class, names(CLASS_COLS)),
         code = factor(uic24, levels = uic_class %>% arrange(factor(class, names(CLASS_COLS)), uic24) %>% pull(uic24))) %>%
  ggplot(aes(x = code, y = beta, colour = class)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_point(size = 3) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_colour_manual(values = CLASS_COLS, name = NULL) +
  labs(title = "The rural advantage step by step, from metro to most remote",
       subtitle = "Each USDA-ERS Urban Influence Code against large-metro credit unions; same size, state and quarter; bars = 95% range",
       x = "Urban Influence Code (2024), ordered from metro to remote", y = "Percentage points",
       caption = "Preliminary. Rural under the statute = the two brown classes.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(file.path(OUT5S, "rq5_gradient_by_uic.png"), p_uic, width = 11, height = 4.8, dpi = 300, bg = "white")

p_dist <- grad_dist %>%
  ggplot(aes(x = dband, y = beta, group = outcome)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#B08D57", alpha = 0.25) +
  geom_line(colour = "#5C4326", linewidth = 1) + geom_point(colour = "#5C4326", size = 2.5) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(title = "The rural advantage by distance from the nearest metro area",
       subtitle = "Against metro credit unions; same size, state and quarter; shaded = 95% range",
       x = "Straight-line distance from the county's centre to the nearest metro area", y = "Percentage points",
       caption = "County-level distances (headquarters county). Preliminary.") +
  theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"))
ggsave(file.path(OUT5S, "rq5_gradient_by_distance.png"), p_dist, width = 11, height = 4.8, dpi = 300, bg = "white")

## ---- S7  save and print the slide numbers ------------------------------------------
write.csv(grad_uic,   file.path(OUT5S, "rq5_gradient_by_uic.csv"),      row.names = FALSE)
write.csv(grad_dist,  file.path(OUT5S, "rq5_gradient_by_distance.csv"), row.names = FALSE)
write.csv(conley_tab, file.path(OUT5S, "rq5_conley.csv"),               row.names = FALSE)

cat("\n=== SLIDE NUMBERS, RQ5 spatial ===\n")
g <- grad_uic %>% filter(outcome == "Asset growth")
cat("Growth advantage vs large metro, by class (mean of codes):\n")
print(g %>% group_by(class) %>% summarise(mean_beta = round(mean(beta), 2), codes = paste(uic24, collapse = ","), .groups = "drop"))
cat(sprintf("Growth per 10 miles from a metro (outside metros, capped at 150 mi): %+.3f\n", coef(slope)[1]))
cat(sprintf("Conley 200 km: growth range %s zero; ROA range %s zero\n",
            ifelse(conley_tab %>% filter(outcome == "g_assets", se_type == "conley_200km") %>% pull(still_above_zero), "stays above", "CROSSES"),
            ifelse(conley_tab %>% filter(outcome == "roa_pct",  se_type == "conley_200km") %>% pull(still_above_zero), "stays above", "CROSSES")))
