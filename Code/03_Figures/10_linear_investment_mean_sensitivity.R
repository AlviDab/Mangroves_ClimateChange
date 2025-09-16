# Author: Alvise Dabalà
# Date: 01/07/2025
# Purpose: Sensitivity analysis of "linear investment" (relationship between area increase and mean resilience increase).
#          Computes area and resilience summaries across climate-smart thresholds, fits simple linear models,
#          produces scatterplots and saves results (xlsx, rds, svg).

################################################################################

pacman::p_load(
  tidyverse,
  sf,
  parallel,
  furrr,
  purrr,
  spatstat,
  collapse,
  modelr,
  openxlsx,
  patchwork
)

# Number of cores to use for parallel processing (leave 2 cores free)
ncores <- detectCores() - 2

# Set furrr multisession plan for parallel execution
plan(multisession, workers = ncores)
set.seed(123)

# List available prioritisations (input folders)
prioritisations_list <- list.files("Results/RDS/prioritisation/Country/01_prioritisation/")

# Keep only scenarios relevant to sensitivity analysis (30% targets or 1000PU)
prioritisations_list <- prioritisations_list[str_detect(prioritisations_list, "30|1000PU")]

# Which climate-change exposure metric to use (mean or median). Here using mean.
CC_direction <- "mean"

# Create plots for both sensitivity settings (uniform targets and 1000 km² PUs)
plots_list <- map(c("targets_30", "targets_area_1000PU"), function(sensitivity) {
  # Within each sensitivity, evaluate both global (biotyp) and country+biotyp scales
  area_cr <- map(c("country_and_biotyp", "biotyp"), function(scale) {
    split_group <- paste0(scale, "_", sensitivity)

    # Iterate thresholds from 0 to 1 (0 means baseline non-CC solution)
    future_map(seq(0, 1, by = 0.05), function(prct) {
      # Read baseline (prct == 0) or climate-smart solutions for prct > 0
      if (prct == 0) {
        solution <- readRDS(
          paste0(
            "Results/RDS/prioritisation/Country/01_prioritisation/",
            split_group,
            "/solution_prioritisation.rds"
          )
        )
      } else {
        solution <- readRDS(
          paste0(
            "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
            split_group,
            "/",
            CC_direction,
            "/solution_",
            as.character(prct),
            "_",
            CC_direction,
            ".rds"
          )
        )
      }

      # Rename the CC metric column to a generic 'resilience' for downstream convenience
      solution <- solution %>%
        rename_with(~"resilience", !!sym(paste0(
          "Prob_gain_stability_", CC_direction
        )))

      # Compute total selected mangrove area (km2) for selected planning units (solution_1 == 1)
      area <- solution %>%
        st_drop_geometry() %>%
        filter(solution_1 == 1) %>%
        summarise(Area_mangrove_km2 = sum(MangroveArea_km2))

      # Compute weighted median and weighted mean of resilience across selected PUs
      # Weight by MangroveArea_km2 so larger PUs contribute proportionally
      climate_resilient_median_mean <- solution %>%
        st_drop_geometry() %>%
        filter(solution_1 == 1) %>%
        summarise(
          median_resilience =
            spatstat.univar::weighted.median(resilience, MangroveArea_km2),
          mean_resilience =
            weighted.mean(resilience, MangroveArea_km2)
        )
      # mode_resilience = collapse::fmode(resilience,
      #                                   MangroveArea_km2))

      # Compute area of selected PUs with resilience >= 75 (example threshold for "high" resilience)
      climate_resilient_75 <- solution %>%
        st_drop_geometry() %>%
        filter(solution_1 == 1, resilience >= 75) %>%
        summarise(area_climate_resilient_75 = sum(MangroveArea_km2))

      # Combine area & resilience summaries and tag with threshold value
      area_cr <- area %>%
        cbind(climate_resilient_median_mean) %>%
        cbind(climate_resilient_75) %>%
        add_column(threshold = prct)
    }) %>%
      bind_rows() %>%
      # Attach scale label and CC metric edge name, then compute percent changes relative to baseline
      mutate(scale = scale, edge = CC_direction) %>%
      mutate(
        # percent increase in area relative to baseline (prct == 0)
        prct_increase_area = (Area_mangrove_km2 -
          Area_mangrove_km2[1]) /
          Area_mangrove_km2[1],
        # percent increase in mean resilience relative to baseline
        prct_increase_mean_resilience = (mean_resilience -
          mean_resilience[1]) /
          mean_resilience[1],
        # ratio of percent increase in resilience to percent increase in area (scaled *100)
        ratio_prct_increase = prct_increase_mean_resilience /
          prct_increase_area * 100
      )
  }) %>%
    bind_rows()

  # Prepare datasets for model fitting (exclude baseline rows where prct_increase_area == 0)
  area_cr_global_mean <- area_cr %>%
    filter(scale == "biotyp") %>%
    filter(prct_increase_area != 0)

  area_cr_country_mean <- area_cr %>%
    filter(scale == "country_and_biotyp") %>%
    filter(prct_increase_area != 0)

  # Fit simple linear models: response is percent increase in mean resilience (%) vs log(percent area increase (%))
  # Using log(area increase) to capture diminishing returns behavior
  fit_global_mean <- lm(
    prct_increase_mean_resilience * 100 ~ log(prct_increase_area *
      100),
    data = area_cr_global_mean
  )
  fit_country_mean <- lm(
    prct_increase_mean_resilience * 100 ~ log(prct_increase_area *
      100),
    data = area_cr_country_mean
  )

  # Determine plot title and tag based on sensitivity setting
  plot_title <- ifelse(sensitivity == "targets_30",
    "Uniform conservation targets (30%)",
    "Planning units of 1000 km²"
  )
  plot_tag <- ifelse(sensitivity == "targets_30", "a", "b")

  # Create scatterplot: x = % increase in area, y = % increase in mean resilience
  scatterplot <- ggplot(
    data = area_cr,
    aes(x = prct_increase_area * 100, y = prct_increase_mean_resilience * 100)
  ) +
    geom_point(aes(
      colour = scale,
      shape = scale,
      alpha = threshold
    ), size = 2) +
    scale_colour_manual(
      values = c("#003049", "#4B86AA"),
      labels = c("Global scale", "Country scale"),
      name = "Climate-smart threshold"
    ) +
    # 1:1 line for reference (equal percent increase in resilience and area)
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = c(16, 17),
      labels = c("Global scale", "Country scale"),
      name = "Climate-smart threshold"
    ) +
    guides(
      colour = guide_legend(override.aes = list(alpha = 1)),
      shape = guide_legend(override.aes = list(alpha = 1)),
      alpha = "none"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.1),
      panel.grid.minor = element_line(colour = "grey90", linewidth = 0.05),
      legend.position = "bottom",
      plot.tag = element_text(face = "bold"),
      title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    ) +
    ylab("Percentage increase in resilience (%)") +
    xlab("Percentage increase in area (%)") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    ggtitle(plot_title) +
    labs(tag = plot_tag)

  # Save per-sensitivity outputs: data, plot object and fitted models
  dir.create(
    paste0(
      "Figures/Country/10_linear_investment/",
      sensitivity,
      "/RDS"
    ),
    recursive = TRUE
  )

  # Export tabular summary to Excel
  write.xlsx(
    area_cr %>%
      st_drop_geometry(),
    paste0(
      "Figures/Country/10_linear_investment/",
      sensitivity,
      "/linear_investment_mean.xlsx"
    )
  )

  # Save ggplot object and model fits as RDS for later reuse
  saveRDS(
    scatterplot,
    paste0(
      "Figures/Country/10_linear_investment/",
      sensitivity,
      "/RDS/linear_investment_mean.rds"
    )
  )
  saveRDS(
    fit_global_mean,
    paste0(
      "Figures/Country/10_linear_investment/",
      sensitivity,
      "/RDS/fit_global_mean.rds"
    )
  )
  saveRDS(
    fit_country_mean,
    paste0(
      "Figures/Country/10_linear_investment/",
      sensitivity,
      "/RDS/fit_country_mean.rds"
    )
  )

  # Return the plot for later combination via patchwork
  return(scatterplot)
})

# Create patchwork plot by stacking the two sensitivity plots vertically with a shared legend
plot_a <- plots_list[[1]] # targets_30
plot_b <- plots_list[[2]] # targets_area_1000PU

combined_plot <- plot_a / plot_b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Ensure output directory exists for combined plot and save SVG + RDS
dir.create("Figures/Country/10_linear_investment/combined/RDS", recursive = TRUE)

ggsave(
  plot = combined_plot,
  "Figures/Country/10_linear_investment/combined/linear_investment_mean_patchwork.svg",
  dpi = 300,
  width = 18,
  height = 20,
  units = "cm"
)

saveRDS(
  combined_plot,
  "Figures/Country/10_linear_investment/combined/RDS/linear_investment_mean_patchwork.rds"
)

# Clean up workspace
rm(list = ls())
gc()
.rs.restartR()
