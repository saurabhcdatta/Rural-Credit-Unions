###############################################################################
# rural_maps.R
#
# Presentation-quality US county maps for the ROAD Act Sec. 909 study.
#
#   M1  Statutory rural classification, 2024 UIC  -- the reference map
#   M2  Reclassification, 2013 UIC -> 2024 UIC    -- what OMB's redelineation did
#   M3  Rural counties with / without a credit union HQ   (needs the panel)
#
# Alaska, Hawaii, and Puerto Rico are repositioned via tigris::shift_geometry()
# and everything is drawn in Albers Equal Area, which is the correct projection
# for a national choropleth -- Mercator badly overstates northern counties.
#
# Outputs 300 dpi PNG (slides) and PDF (vector, for print).
###############################################################################

library(data.table)
library(ggplot2)
library(sf)
library(tigris)

source("rural_definition.R")

options(tigris_use_cache = TRUE)   # county shapefile is ~50MB; cache it
sf::sf_use_s2(FALSE)

CFG <- list(cache_dir = "data/raw", out_dir = "out/maps", dpi = 300)
dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------------------------
## Palette and theme
## ---------------------------------------------------------------------------
## Colourblind-safe. The two "stable" categories are deliberately muted so the
## changed counties carry all the visual weight in M2.

PAL <- c(
  "Rural"               = "#4E7A51",   # green
  "Not rural"           = "#DCE1E6",   # near-white grey
  "Stable rural"        = "#A9C4AC",   # muted green
  "Stable non-rural"    = "#E8ECEF",   # pale grey
  "Lost rural status"   = "#C4451C",   # burnt orange
  "Gained rural status" = "#1F6FB2"    # blue
)

theme_map <- function(base = 11) {
  theme_void(base_size = base) +
    theme(
      plot.title      = element_text(face = "bold", size = base * 1.5,
                                     colour = "#10263B", hjust = 0,
                                     margin = margin(b = 4)),
      plot.subtitle   = element_text(size = base * 1.0, colour = "#4A5A6A",
                                     hjust = 0, margin = margin(b = 12), lineheight = 1.2),
      plot.caption    = element_text(size = base * 0.75, colour = "#7A8894",
                                     hjust = 0, margin = margin(t = 12), lineheight = 1.2),
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = base * 0.95, colour = "#2C3E50"),
      legend.key.width  = unit(20, "pt"),
      legend.key.height = unit(11, "pt"),
      plot.margin     = margin(16, 16, 12, 16),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

save_map <- function(p, name, w = 11, h = 7.4) {
  ggsave(file.path(CFG$out_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = CFG$dpi, bg = "white")
  ggsave(file.path(CFG$out_dir, paste0(name, ".pdf")), p,
         width = w, height = h, device = cairo_pdf, bg = "white")
  message("Saved ", name, ".png / .pdf")
}

## ---------------------------------------------------------------------------
## Geometry + classification
## ---------------------------------------------------------------------------
## cb = TRUE gives the generalized cartographic boundary file -- the right
## choice for a national map. The full TIGER file is far too detailed and
## renders slowly without looking any better at this scale.

message("Fetching county geometry (cached after the first run)...")
cty <- counties(cb = TRUE, resolution = "5m", year = 2024, progress_bar = FALSE)
cty <- shift_geometry(cty, preserve_area = FALSE, position = "below")
setDT(cty_dt <- as.data.table(sf::st_drop_geometry(cty)))

sts <- states(cb = TRUE, resolution = "5m", year = 2024, progress_bar = FALSE)
sts <- shift_geometry(sts, preserve_area = FALSE, position = "below")
sts <- sts[!sts$STUSPS %in% c("AS","GU","MP","VI"), ]

uic <- uic_setup(cache_dir = CFG$cache_dir)

cty$fips <- cty$GEOID
map_dt <- merge(cty, as.data.frame(uic$u2024[, .(fips, rural_2024 = rural, uic_2024 = uic, pop)]),
                by = "fips", all.x = TRUE)
map_dt <- merge(map_dt, as.data.frame(uic$u2013[, .(fips, rural_2013 = rural)]),
                by = "fips", all.x = TRUE)

## ---------------------------------------------------------------------------
## M1. Statutory rural classification
## ---------------------------------------------------------------------------
m1_dt <- map_dt[!is.na(map_dt$rural_2024), ]
m1_dt$cat <- factor(ifelse(m1_dt$rural_2024 == 1, "Rural", "Not rural"),
                    levels = c("Rural", "Not rural"))
n_rural <- sum(m1_dt$rural_2024 == 1, na.rm = TRUE)
pop_sh  <- 100 * sum(m1_dt$pop[m1_dt$rural_2024 == 1], na.rm = TRUE) /
                 sum(m1_dt$pop, na.rm = TRUE)

m1 <- ggplot(m1_dt) +
  geom_sf(aes(fill = cat), colour = NA) +
  geom_sf(data = sts, fill = NA, colour = "white", linewidth = 0.28) +
  scale_fill_manual(values = PAL, drop = FALSE,
                    labels = c(sprintf("Rural  (%s counties)", format(n_rural, big.mark = ",")),
                               "Not rural")) +
  labs(title = "Which counties are \"rural\" under the ROAD Act",
       subtitle = sprintf(
         "12 CFR 1026.35(b)(2)(iv)(A)(1): a county in neither a metropolitan statistical area nor a\nmicropolitan area adjacent to one. %s of 3,144 counties — %.0f%% — hold %.1f%% of the population.",
         format(n_rural, big.mark = ","), 100 * n_rural / 3144, pop_sh),
       caption = "Source: USDA Economic Research Service, 2024 Urban Influence Codes (UIC 3, 6, 7, 8, 9).\nAlaska, Hawaii, and Puerto Rico repositioned. Albers Equal Area projection.") +
  theme_map()

save_map(m1, "M1_rural_classification")

## ---------------------------------------------------------------------------
## M2. Reclassification, 2013 -> 2024 vintage
## ---------------------------------------------------------------------------
## The point of this map: none of these counties changed. The map did.

m2_dt <- map_dt[!is.na(map_dt$rural_2024) & !is.na(map_dt$rural_2013), ]
m2_dt$cat <- with(m2_dt, factor(
  ifelse(rural_2013 == 1 & rural_2024 == 1, "Stable rural",
  ifelse(rural_2013 == 0 & rural_2024 == 0, "Stable non-rural",
  ifelse(rural_2013 == 1 & rural_2024 == 0, "Lost rural status", "Gained rural status"))),
  levels = c("Lost rural status", "Gained rural status", "Stable rural", "Stable non-rural")))

n <- table(m2_dt$cat)
m2 <- ggplot(m2_dt) +
  geom_sf(aes(fill = cat), colour = NA) +
  geom_sf(data = sts, fill = NA, colour = "white", linewidth = 0.28) +
  scale_fill_manual(values = PAL, drop = FALSE,
                    labels = sprintf("%s  (%d)", names(n), as.integer(n))) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Counties that changed rural status without changing at all",
       subtitle = sprintf(
         "Applying the 2013 versus the 2024 Urban Influence Codes moves %d counties across the statutory line.\nThe institutions in them are reclassified with no change in their business — a definitional artifact, not a trend.",
         sum(n[c("Lost rural status","Gained rural status")])),
       caption = "Source: USDA Economic Research Service, 2013 and 2024 Urban Influence Codes.\nChanges stem from OMB's July 2023 redelineation of core-based statistical areas, not from the ERS code restructuring:\nthe 2013-to-2024 crosswalk is rural-status preserving. Connecticut excluded — counties were replaced by planning regions in 2022.") +
  theme_map()

save_map(m2, "M2_reclassification")

## ---------------------------------------------------------------------------
## M3. Rural counties with and without a credit union headquarters
## ---------------------------------------------------------------------------
## Optional -- needs the Call Report panel. This is the "depository desert"
## framing, and it is usually the map executives remember.

if (file.exists("panel_prep.R") && exists("PANEL_DIR")) {
  source("panel_prep.R")
  P  <- prep_panel(PANEL_DIR, cache_dir = CFG$cache_dir)
  cu <- P$cr[qidx == max(qidx), .(cus = uniqueN(cu_number)), by = fips]

  m3_dt <- merge(m1_dt, as.data.frame(cu), by = "fips", all.x = TRUE)
  m3_dt$cus[is.na(m3_dt$cus)] <- 0
  m3_dt <- m3_dt[m3_dt$rural_2024 == 1, ]
  m3_dt$cat <- factor(ifelse(m3_dt$cus > 0, "Rural", "Not rural"),
                      levels = c("Rural", "Not rural"))
  n_with <- sum(m3_dt$cus > 0)

  m3 <- ggplot(m3_dt) +
    geom_sf(aes(fill = cat), colour = NA) +
    geom_sf(data = sts, fill = NA, colour = "white", linewidth = 0.28) +
    scale_fill_manual(values = PAL,
                      labels = c(sprintf("Has a credit union headquarters  (%d)", n_with),
                                 sprintf("None  (%d)", nrow(m3_dt) - n_with))) +
    labs(title = "Rural counties with no credit union headquartered in them",
         subtitle = sprintf("%d of %d rural counties — %.0f%% — have no headquartered credit union.\nHeadquarters is not the same as service: branches and field of membership extend further. Treat this as an upper bound on absence.",
                            nrow(m3_dt) - n_with, nrow(m3_dt),
                            100 * (nrow(m3_dt) - n_with) / nrow(m3_dt)),
         caption = "Source: NCUA 5300 Call Report; USDA ERS 2024 Urban Influence Codes. Non-rural counties not shown.") +
    theme_map()

  save_map(m3, "M3_rural_cu_presence")
} else {
  message("\nM3 skipped. To draw it, set PANEL_DIR <- \"path/to/dta\" and re-run.")
}

message("\nMaps written to ", normalizePath(CFG$out_dir))
