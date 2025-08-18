#Author: Alvise Dabalà
#Date: 01/07/2025

pacman::p_load(tidyverse,
               sf,
               parallel,
               furrr,
               purrr,
               spatstat,
               collapse,
               modelr,
               openxlsx,
               patchwork)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

prioritisations_list <- list.files("Results/resp_v1/RDS/prioritisation/Country/01_prioritisation/")

# Keep only results to use for the sensitivity analysis
prioritisations_list <- prioritisations_list[str_detect(prioritisations_list, "30|1000PU")]

CC_direction <- "mean"

# Create plots for both sensitivity analyses
plots_list <- map(c("targets_30", "targets_area_1000PU"), function(sensitivity) {
  area_cr <- map(c("country_and_biotyp", "biotyp"), function(scale) {

    split_group <- paste0(scale, "_", sensitivity)

    future_map(seq(0, 1, by = 0.05), function(prct) {
      if (prct == 0) {
        solution <- readRDS(
          paste0(
            "Results/resp_v1/RDS/prioritisation/Country/01_prioritisation/",
            split_group,
            "/solution_prioritisation.rds"
          )
        )
      } else {
        solution <- readRDS(
          paste0(
            "Results/resp_v1/RDS/prioritisation/Country/02_prioritisation_CC/",
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

      solution <- solution %>%
        rename_with(~ "resilience", !!sym(paste0(
          "Prob_gain_stability_", CC_direction
        )))

      area <- solution %>%
        st_drop_geometry() %>%
        filter(solution_1 == 1) %>%
        summarise(Area_mangrove_km2 = sum(MangroveArea_km2))

      #Median resilience
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

      #Filter mangroves with resilience >= 75
      climate_resilient_75 <- solution %>%
        st_drop_geometry() %>%
        filter(solution_1 == 1, resilience >= 75) %>%
        summarise(area_climate_resilient_75 = sum(MangroveArea_km2))

      area_cr <- area %>%
        cbind(climate_resilient_median_mean) %>%
        cbind(climate_resilient_75) %>%
        add_column(threshold = prct)
    }) %>%
      bind_rows() %>%
      mutate(scale = scale, edge = CC_direction) %>%
      mutate(
        prct_increase_area = (Area_mangrove_km2 -
                                Area_mangrove_km2[1]) /
          Area_mangrove_km2[1],
        prct_increase_mean_resilience = (mean_resilience -
                                           mean_resilience[1]) /
          mean_resilience[1],
        ratio_prct_increase = prct_increase_mean_resilience /
          prct_increase_area * 100
      )
  }) %>%
    bind_rows()

  area_cr_global_mean <- area_cr %>%
    filter(scale == "biotyp") %>%
    filter(prct_increase_area != 0)

  area_cr_country_mean <- area_cr %>%
    filter(scale == "country_and_biotyp") %>%
    filter(prct_increase_area != 0)

  fit_global_mean <- lm(prct_increase_mean_resilience * 100 ~ log(prct_increase_area *
                                                                    100),
                        data = area_cr_global_mean)
  fit_country_mean <- lm(prct_increase_mean_resilience * 100 ~ log(prct_increase_area *
                                                                     100),
                         data = area_cr_country_mean)

  # Determine plot title and tag
  plot_title <- ifelse(sensitivity == "targets_30",
                       "Uniform conservation targets (30%)",
                       "Planning units of 1000 km²")
  plot_tag <- ifelse(sensitivity == "targets_30", "a", "b")

  scatterplot <- ggplot(data = area_cr,
                        aes(x = prct_increase_area * 100, y = prct_increase_mean_resilience * 100)) +
    geom_point(aes(
      colour = scale,
      shape = scale,
      alpha = threshold
    ), size = 2) +
    scale_colour_manual(
      values = c("#003049", "#4B86AA"),
      labels = c('Global scale', 'Country scale'),
      name = "Climate-smart threshold"
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = c(16, 17),
      labels = c('Global scale', 'Country scale'),
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
      legend.position = 'bottom',
      plot.tag = element_text(face = 'bold'),
      title = element_text(size = 12, face = 'bold'),
      legend.title = element_text(size = 12, face = 'bold'),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    ) +
    ylab("Percentage increase in resilience (%)") +
    xlab("Percentage increase in area (%)") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    ggtitle(plot_title) +
    labs(tag = plot_tag)

  # Save individual plot data and fits
  dir.create(
    paste0(
      "Figures/resp_v1/Country/10_linear_investment/",
      sensitivity,
      "/RDS"
    ),
    recursive = TRUE
  )

  write.xlsx(
    area_cr %>%
      st_drop_geometry(),
    paste0(
      "Figures/resp_v1/Country/10_linear_investment/",
      sensitivity,
      "/linear_investment_mean.xlsx"
    )
  )

  saveRDS(
    scatterplot,
    paste0("Figures/resp_v1/Country/10_linear_investment/",
           sensitivity,
           "/RDS/linear_investment_mean.rds")
  )
  saveRDS(
    fit_global_mean,
    paste0("Figures/resp_v1/Country/10_linear_investment/",
           sensitivity,
           "/RDS/fit_global_mean.rds")
  )
  saveRDS(
    fit_country_mean,
    paste0("Figures/resp_v1/Country/10_linear_investment/",
           sensitivity,
           "/RDS/fit_country_mean.rds")
  )

  # Return the plot for patchwork
  return(scatterplot)
})

# Create patchwork plot
plot_a <- plots_list[[1]]  # targets_30
plot_b <- plots_list[[2]]  # targets_area_1000PU

# Combine plots vertically with shared legend
combined_plot <- plot_a / plot_b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Create output directory for combined plot
dir.create("Figures/resp_v1/Country/10_linear_investment/combined/RDS", recursive = TRUE)

# Save combined plot
ggsave(
  plot = combined_plot,
  "Figures/resp_v1/Country/10_linear_investment/combined/linear_investment_mean_patchwork.svg",
  dpi = 300,
  width = 18,
  height = 20,
  units = "cm"
)

saveRDS(
  combined_plot,
  "Figures/resp_v1/Country/10_linear_investment/combined/RDS/linear_investment_mean_patchwork.rds"
)