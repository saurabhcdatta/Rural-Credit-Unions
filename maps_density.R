###############################################################################
# maps_density.R  --  Maps 13 and 14: adding population density to distance
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
#   Map 13  Distance x density, 3 x 3 bivariate. Rural counties in thirds of
#           (a) population-weighted distance to the nearest office and
#           (b) residents per square mile. "Far and dense" is the policy cell:
#           many people, no office nearby. "Far and sparse" is expected emptiness.
#   Map 14  Proportional symbols. One circle per rural county at its population
#           centre, sized by residents beyond 10 miles, coloured by distance.
#           Same data as map 12, drawn so the eye weights people rather than acres.
#
# REQUIRES: maps.R base-geometry block and maps_distance.R already run in this
#           session (needs cty_sf, st_sf, TH, leg, CAP, sv, fac, cd, K2, cenco,
#           county, palette constants).
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2) })
stopifnot(exists("cd"), exists("K2"), exists("cty_sf"), exists("cenco"))

## ---- E1  density: residents per square mile from the county geometry --------
area <- data.table(fips = cty_sf$fips, sqmi = as.numeric(cty_sf$ALAND) / 2589988.11)
area[fips %in% c("02063", "02066"), fips := "02261"]
area <- area[, .(sqmi = sum(sqmi)), by = fips]
D <- merge(cd[, .(fips, pop, miles_wmean, share_over_10, rural24)], area, by = "fips", all.x = TRUE)
D <- D[rural24 == 1L & !is.na(sqmi) & sqmi > 0]
D[, density := pop / sqmi]
cat("Rural counties with density:", nrow(D), "\n")
print(D[, .(median_density = round(median(density), 1), median_miles = round(median(miles_wmean), 1))])   ## LOOK

## ---- E2  thirds on each axis, named with their cut points ------------------
qd <- D[, quantile(miles_wmean, c(1/3, 2/3))]; qn <- D[, quantile(density, c(1/3, 2/3))]
D[, dist3 := fifelse(miles_wmean <= qd[1], "near", fifelse(miles_wmean <= qd[2], "mid", "far"))]
D[, dens3 := fifelse(density <= qn[1], "sparse", fifelse(density <= qn[2], "medium", "dense"))]
lab_d <- c(near = sprintf("near (under %.0f mi)", qd[1]), mid = sprintf("mid (%.0f\u2013%.0f mi)", qd[1], qd[2]), far = sprintf("far (over %.0f mi)", qd[2]))
lab_n <- c(sparse = sprintf("sparse (under %.0f/sq mi)", qn[1]), medium = sprintf("medium (%.0f\u2013%.0f/sq mi)", qn[1], qn[2]), dense = sprintf("dense (over %.0f/sq mi)", qn[2]))
D[, cell := paste(lab_d[dist3], "\u00B7", lab_n[dens3])]

## 3 x 3 bivariate palette: rows = distance (near -> far), columns = density (sparse -> dense).
## Blue-ish grows with density, red-ish with distance; the far+dense corner is darkest.
biv9 <- c("near|sparse" = "#E8E8E8", "near|medium" = "#B5C0DA", "near|dense" = "#6C83B5",
          "mid|sparse"  = "#E4C9B3", "mid|medium"  = "#B39A9E", "mid|dense"  = "#6B6F8E",
          "far|sparse"  = "#D9A28A", "far|medium"  = "#B1777A", "far|dense"  = "#6E3A5E")
lv <- as.vector(t(outer(c("near","mid","far"), c("sparse","medium","dense"), paste, sep = "|")))
D[, key := paste(dist3, dens3, sep = "|")]
pal <- setNames(biv9[lv], paste(lab_d[sub("\\|.*", "", lv)], "\u00B7", lab_n[sub(".*\\|", "", lv)]))
D[, cell := fac(cell, names(pal))]

## ---- MAP 13 -------------------------------------------------------------------
K13 <- merge(county[is_state_or_dc == TRUE, .(fips, rural24)],
             rbind(D[, .(fips, cell)], D[fips == "02261", .(fips = c("02063", "02066"), cell)]), by = "fips", all.x = TRUE)
K13[rural24 == 0L, cell := NA]
M13 <- merge(cty_sf, K13, by = "fips", all.x = TRUE)
fd <- D[dist3 == "far" & dens3 == "dense"]
p13 <- ggplot() +
  geom_sf(data = M13, aes(fill = cell), colour = C_CTY, linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = pal, breaks = names(pal), drop = FALSE, na.value = C_NONRURAL,
                    name = "Distance to nearest credit union office  \u00B7  Residents per square mile   (thirds among rural counties)") +
  labs(title = sprintf("%d rural counties are both far from a credit union office and comparatively densely settled\n\u2014 %.1f million people in the corner that is not explained by emptiness", nrow(fd), fd[, sum(pop)] / 1e6),
       subtitle = "Read across for density, down for distance. Grey = expected: few people, far from an office. Dark purple = the policy problem: many people, far from an office.",
       caption = CAP("Distance from Census 2020 tract population centres to ZIP-centroid office locations. Density from Census county land area and 2024 population.")) + TH + leg(3)
sv(p13, "map13_distance_x_density")
fwrite(fd[order(-pop), .(fips, pop, miles_wmean = round(miles_wmean, 1), density = round(density, 1), share_over_10 = round(100 * share_over_10, 1))],
       file.path(MAP_DIR, "far_and_dense_counties.csv"))
cat("Far-and-dense list written:", nrow(fd), "counties\n")                                             ## LOOK -- these are names for the case studies

## ---- MAP 14  proportional symbols: residents beyond 10 miles ----------------------
S <- merge(D[, .(fips, pop, share_over_10, miles_wmean)], cenco, by = "fips")
S[, beyond10 := pop * share_over_10]
S <- S[beyond10 > 0]
S_sf <- st_as_sf(S, coords = c("lon", "lat"), crs = 4326) |> tigris::shift_geometry()
S_dt <- cbind(as.data.table(st_drop_geometry(S_sf)), st_coordinates(S_sf))
S_dt[, dist_bin := fac(as.character(cut(miles_wmean, c(-Inf, 10, 20, 40, Inf), labels = c("Under 10 miles", "10-20 miles", "20-40 miles", "Over 40 miles"))),
                       c("Under 10 miles", "10-20 miles", "20-40 miles", "Over 40 miles"))]
base14 <- merge(cty_sf, county[is_state_or_dc == TRUE, .(fips, rural24)], by = "fips", all.x = TRUE)
base14$base <- ifelse(base14$rural24 == 1L, "Rural county", "Non-rural county")
p14 <- ggplot() +
  geom_sf(data = base14, aes(fill = base), colour = C_CTY, linewidth = 0.05, show.legend = FALSE) +
  scale_fill_manual(values = c("Rural county" = "#F1EEE4", "Non-rural county" = C_NONRURAL)) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  geom_point(data = S_dt[order(-beyond10)], aes(X, Y, size = beyond10, colour = dist_bin), alpha = 0.75, shape = 16) +
  scale_size_area(max_size = 9, breaks = c(5000, 20000, 50000, 100000), labels = c("5,000", "20,000", "50,000", "100,000"),
                  name = "Residents more than 10 miles from an office") +
  scale_colour_manual(values = c("Under 10 miles" = "#7FB0AE", "10-20 miles" = "#E8A24A", "20-40 miles" = "#C8553D", "Over 40 miles" = "#7A2A1A"),
                      name = "Average distance in the county") +
  labs(title = sprintf("Where the %.1f million rural residents beyond 10 miles actually live", S_dt[, sum(beyond10)] / 1e6),
       subtitle = "One circle per rural county at its population centre, sized by residents more than 10 miles from any credit union office. Drawn so people, not acres, carry the visual weight.",
       caption = CAP("Distance from Census 2020 tract population centres to ZIP-centroid office locations. Circles at Census county population centroids.")) +
  TH + guides(size = guide_legend(nrow = 1, title.position = "top", override.aes = list(colour = "#5B6B72")),
              colour = guide_legend(nrow = 1, title.position = "top", override.aes = list(size = 4)))
sv(p14, "map14_people_beyond_10_miles")

cat("\nMaps 13 and 14 written.\n")
