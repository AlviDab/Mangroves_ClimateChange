# Author: Alvise Dabalà
# Date: 01/07/2025
# Purpose: Create maps comparing landward vs seaward prioritisation selections,
#          compute agreement (kappa) per country and visualise area-selection differences.
#          Save per-scenario plots and a combined figure.

################################################################################

pacman::p_load(tidyverse, sf, purrr, furrr, parallel, openxlsx)

# Proportion target used in filename construction (e.g. 0.3 -> 30%)
prct <- 0.3

# Two grouping scenarios to iterate over: uniform 30% targets and 1000 km² planning units
split_group_names <- c("biotyp_targets_30", "biotyp_targets_area_1000PU")

# Load helper functions: world map creation and country intersection helper
source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()
source("Code/Functions/f_intersect_countries.R")

# For each grouping scenario, read in the seaward and landward solutions,
# calculate resilience values, kappa statistics, and create maps
map(split_group_names, function(split_group) {
  # Read in solutions for seaward and landward prioritisation for this split_group & prct
  solution_cc_sw <- readRDS(
    paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group,
      "/seaward/solution_",
      as.character(prct),
      "_seaward.rds"
    )
  ) %>%
    mutate(type = "seaward") %>%
    f_int_countries() # ensure solution geometries are intersected by country

  solution_cc_lw <- readRDS(
    paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group,
      "/landward/solution_",
      as.character(prct),
      "_landward.rds"
    )
  ) %>%
    mutate(type = "landward") %>%
    f_int_countries() # same intersection for landward

  # Resilience summaries: weighted means of stability probability for selected planning units
  # Note: filtering solution_1 == TRUE extracts selected planning units only
  solution_cc_sw %>%
    st_drop_geometry() %>%
    filter(solution_1 == TRUE) %>%
    summarise(
      lw_w_mean_resilience = weighted.mean(Prob_gain_stability_landward, MangroveArea_km2),
      sw_w_mean_resilience = weighted.mean(Prob_gain_stability_seaward, MangroveArea_km2)
    )

  solution_cc_lw %>%
    st_drop_geometry() %>%
    filter(solution_1 == TRUE) %>%
    summarise(
      lw_w_mean_resilience = weighted.mean(Prob_gain_stability_landward, MangroveArea_km2),
      sw_w_mean_resilience = weighted.mean(Prob_gain_stability_seaward, MangroveArea_km2)
    )

  # Kappa by country: compute agreement between landward and seaward solutions per country
  kappa_by_country <- map(unique(solution_cc_lw$country), function(sel_country) {
    kappa <- spatialplanr::splnr_get_kappaCorrData(
      list(
        solution_cc_lw %>%
          filter(country == sel_country),
        solution_cc_sw %>%
          filter(country == sel_country)
      ),
      c("landward", "seaward")
    )[[2]] # the function returns multiple items; the second contains the kappa value
  }) %>%
    unlist() %>%
    tibble(
      country = unique(solution_cc_lw$country),
      kappa = .
    )

  # Compute per-country selected area and weighted means, then percentage selected
  solution_cc_separate <- solution_cc_sw %>%
    rbind(solution_cc_lw) %>%
    group_by(country, solution_1, type) %>%
    summarise(
      tot_area = sum(area_km2),
      w_mean_cc_exp_sw = weighted.mean(Prob_gain_stability_seaward, area_km2),
      w_mean_cc_exp_lw = weighted.mean(Prob_gain_stability_landward, area_km2)
    ) %>%
    pivot_wider(
      names_from = "solution_1",
      values_from = c("tot_area", "w_mean_cc_exp_sw", "w_mean_cc_exp_lw")
    ) %>%
    group_by(country, type) %>%
    summarise(across(ends_with(c("0", "1")), ~ sum(., na.rm = TRUE))) %>%
    mutate(perc_sel_area = tot_area_1 / (tot_area_1 + tot_area_0)) %>%
    ungroup()

  # Combine landward/seaward percentages and attach kappa values
  solution_cc <- solution_cc_separate %>% # Maybe add also the exposure to climate change
    pivot_wider(names_from = type, values_from = perc_sel_area) %>%
    dplyr::select(country, landward, seaward) %>%
    left_join(kappa_by_country, by = "country") %>%
    group_by(country) %>%
    summarise(
      country = first(country),
      landward = sum(landward, na.rm = TRUE),
      seaward = sum(seaward, na.rm = TRUE),
      kappa = first(kappa)
    ) %>%
    mutate(
      # difference in proportion selected (landward - seaward)
      ratio_lw_sw = landward - seaward,
      # size for plotting dots: absolute difference scaled to percent for visualization
      size_dots = abs(ratio_lw_sw) * 100
    ) %>%
    st_centroid() %>% # place points at country centroids for mapping
    mutate(
      # convert numeric kappa to descriptive categories for legend
      kappa_def = case_when(
        kappa == 1 ~ "Perfect",
        kappa == 0.2 ~ "None to slight",
        # ranges for intermediate categories
        dplyr::between(kappa, 0.8, 1) ~ "Almost perfect",
        dplyr::between(kappa, 0.6, 0.8) ~ "Substantial",
        dplyr::between(kappa, 0.4, 0.6) ~ "Moderate",
        dplyr::between(kappa, 0.2, 0.4) ~ "Fair",
        .default = "None to slight"
      )
    ) %>%
    # hide dot sizes for countries without kappa (NA)
    mutate(size_dots = ifelse(is.na(kappa), NA, size_dots))

  # Global boundary for plotting context
  dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

  # Build ggplot: world basemap, country points sized by area difference and coloured by agreement
  plot_map <- ggplot() +
    geom_sf(
      data = world_map,
      fill = "grey60",
      colour = "grey60",
      linewidth = 0.001
    ) +
    geom_sf(
      data = solution_cc,
      aes(colour = kappa_def, size = size_dots),
      alpha = 0.8
    ) +
    labs(size = "Area difference %") +
    scale_colour_viridis_d(
      option = "D",
      direction = -1,
      name = "Agreement",
      limits = c(
        "None to slight",
        "Fair",
        "Moderate",
        "Substantial",
        "Almost perfect",
        "Perfect"
      )
    ) +
    geom_sf(data = dat, fill = NA) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major = element_line(colour = "transparent"),
      panel.background = element_blank(),
      legend.position = "top",
      legend.box = "vertical",
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_alpha_continuous(range = c(0.8, 1)) +
    coord_sf(datum = NA)

  # Ensure output directory exists for this split_group
  dir.create(
    paste0(
      "Figures/Country/09_map_kappa_ratio_lw_sw/",
      split_group,
      "/RDS"
    ),
    recursive = TRUE
  )

  # Save per-split_group PDF (small figure)
  ggsave(
    paste0(
      "Figures/Country/09_map_kappa_ratio_lw_sw/",
      split_group,
      "/map_ratio_lw_sw_",
      prct,
      ".pdf"
    ),
    dpi = 300,
    width = 18,
    height = 11,
    units = "cm"
  )

  # Export tabular results to Excel for further inspection (one sheet per append call)
  write.xlsx(
    solution_cc %>%
      st_drop_geometry(),
    append = TRUE,
    file = paste0(
      "Figures/Country/09_map_kappa_ratio_lw_sw/",
      split_group,
      "/map_kappa_ratio_lw_sw_",
      prct,
      ".xlsx"
    )
  )

  # Save ggplot object to RDS for later combination
  saveRDS(
    plot_map,
    paste0(
      "Figures/Country/09_map_kappa_ratio_lw_sw/",
      split_group,
      "/RDS/map_ratio_lw_sw_",
      prct,
      ".rds"
    )
  )
})

# Combine the two saved plots (one per split_group) using patchwork
library(patchwork)
titles <- c("Uniform conservation targets (30%)", "Planning units of 1000 km²")

(plot_map <- map2(split_group_names, titles, function(split_group, plot_title) {
  plot_map <- readRDS(
    paste0(
      "Figures/Country/09_map_kappa_ratio_lw_sw/",
      split_group,
      "/RDS/map_ratio_lw_sw_",
      prct,
      ".rds"
    )
  )
  plot_map +
    labs(title = plot_title)
}) %>%
  wrap_plots(ncol = 1) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.tag = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 12, hjust = 0)
  ) &
  plot_annotation(tag_levels = "a")
)

# Save combined figure as SVG for publication-quality vector output
ggsave(
  "Figures/Country/09_map_kappa_ratio_lw_sw/map_ratio_lw_sw.svg",
  plot = plot_map,
  dpi = 300,
  width = 18,
  height = 22,
  units = "cm"
)

# Clean up environment and free memory at end of script
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects
gc() # free up memory and report usage
.rs.restartR()
