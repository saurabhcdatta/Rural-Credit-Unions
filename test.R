## Rural only, latest four quarters: net worth by county population trend and by loan demand
d %>%
  filter(rural == 1, qidx > QZ - 4, !is.na(county_pop_change_pct), !is.na(loan_to_share)) %>%
  mutate(pop_trend  = ntile(county_pop_change_pct, 3),
         loan_demand = ntile(loan_to_share, 3)) %>%
  group_by(pop_trend, loan_demand) %>%
  summarise(credit_unions = n_distinct(cu_number),
            median_networth = round(median(networth_pct), 1),
            median_growth   = round(median(g_assets, na.rm = TRUE), 2),
            .groups = "drop") %>%
  print(n = 9)

## Same question in the regression: does the capital effect get stronger where demand is weak?
m_dem <- feols(g_assets ~ networth_lag * loan_to_share + networth_lag * county_pop_change_pct
               + roa_lag + ln_assets + ln_pop + ln_density + ln_offices | state_quarter,
               data = reg_data %>% filter(rural == 1, !is.na(roa_lag)), cluster = ~cu_number)
summary(m_dem)

rd <- reg_data %>% filter(rural == 1, !is.na(roa_lag)) %>%
  mutate(lts_c = loan_to_share - median(loan_to_share),
         pop_c = county_pop_change_pct - median(county_pop_change_pct, na.rm = TRUE))
m_dem2 <- feols(g_assets ~ networth_lag * lts_c + networth_lag * pop_c
                + roa_lag + ln_assets + ln_pop + ln_density + ln_offices | state_quarter,
                data = rd, cluster = ~cu_number)
summary(m_dem2)