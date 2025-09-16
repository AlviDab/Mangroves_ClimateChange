# Author: Alvise Dabalà
# Date: 28/05/2024
# Description: This script creates density plots comparing the resilience of mangroves
#             selected in the prioritisation solutions with the resilience of
#             mangroves within protected areas (WDPA).

###################################################################################

# Load packages
pacman::p_load(tidyverse, sf, MetBrewer, purrr, patchwork)

# Load functions and data
source("Code/Functions/f_addcols_WDPA.r")

# Set parameters
prct <- 0.3
targets <- "targets_area"

# Create density plots
map(c(
  "country_and_biotyp",
  "biotyp"
), function(split_group) {
  plot_density <- map(c("mean", "landward", "seaward"), function(CC_direction) {
    # Load solutions
    solution <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/01_prioritisation/",
      split_group, "_", targets, "/solution_prioritisation.rds"
    )) %>%
      f_addcols_WDPA() %>%
      rename_with(~"resilience", !!sym(paste0("Prob_gain_stability_", CC_direction)))

    solution_cc <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group, "_", targets, "/",
      CC_direction, "/solution_",
      as.character(prct), "_", CC_direction, ".rds"
    )) %>%
      f_addcols_WDPA() %>%
      rename_with(~"resilience", !!sym(paste0("Prob_gain_stability_", CC_direction)))

    # Repeat the value for each km² of mangroves selected (we included all the observation > 0.1 km²)
    resilience_priorities <- solution_cc %>%
      st_drop_geometry() %>%
      filter(solution_1 == 1) %>%
      dplyr::select(ID, MangroveArea_km2, resilience) %>%
      mutate(weighted_mean_exposure = weighted.mean(
        resilience,
        MangroveArea_km2
      )) %>%
      mutate(category = "Selected_priorities")

    # Resilience of mangroves in protected areas (WDPA)
    resilience_WDPA_all <- solution_cc %>%
      st_drop_geometry() %>%
      mutate(area_mangroves_WDPA_all_km2 = rowSums(select(., ends_with("WDPA_all_km2")))) %>%
      dplyr::select(ID, area_mangroves_WDPA_all_km2, resilience) %>%
      rename(MangroveArea_km2 = area_mangroves_WDPA_all_km2) %>%
      mutate(weighted_mean_exposure = weighted.mean(
        resilience,
        MangroveArea_km2
      )) %>%
      mutate(category = "WDPA_all")

    # resilience_WDPA_I_VI <- solution_cc %>%
    #   st_drop_geometry() %>%
    #   dplyr::select(ID, area_mangroves_WDPA_I_VI_km2, resilience) %>%
    #   rename(MangroveArea_km2 = area_mangroves_WDPA_I_VI_km2) %>%
    #   mutate(weighted_mean_exposure = weighted.mean(resilience,
    #                                                 MangroveArea_km2)) %>%
    #   mutate(category = "WDPA_I_VI")
    #
    # resilience_WDPA_I_IV <- solution_cc %>%
    #   st_drop_geometry() %>%
    #   dplyr::select(ID, area_mangroves_WDPA_I_IV_km2, resilience) %>%
    #   rename(MangroveArea_km2 = area_mangroves_WDPA_I_IV_km2) %>%
    #   mutate(weighted_mean_exposure = weighted.mean(resilience,
    #                                                 MangroveArea_km2)) %>%
    #   mutate(category = "WDPA_I_IV")

    # Combine data for plotting
    plot_data <- resilience_priorities %>%
      rbind(resilience_WDPA_all) %>%
      # rbind(resilience_WDPA_I_VI) %>%
      # rbind(resilience_WDPA_I_IV) %>%
      mutate(category = case_when(category == "WDPA_all" ~ "Protected Areas",
        # category == "WDPA_I_VI" ~ "Category I-VI",
        # category == "WDPA_I_IV" ~ "Category I-IV",
        .default = "Selected mangroves global-scale climate-smart prioritisation"
      ))

    # Create density plot
    plot_density <- ggplot(data = plot_data) +
      geom_density(
        aes(
          colour = fct_relevel(
            category,
            c(
              "Protected Areas",
              # "Category I-VI", "Category I-IV",
              "Selected mangroves global-scale climate-smart prioritisation"
            )
          ),
          fill = fct_relevel(
            category,
            c(
              "Protected Areas",
              # "Category I-VI", "Category I-IV",
              "Selected mangroves global-scale climate-smart prioritisation"
            )
          ),
          x = resilience,
          weight = MangroveArea_km2
        ),
        alpha = 0.3
      ) +
      geom_vline(aes(
        xintercept = weighted_mean_exposure,
        colour = category
      )) +
      scale_fill_met_d(name = "Egypt", override.order = TRUE) +
      scale_colour_met_d(name = "Egypt", override.order = TRUE) +
      theme_bw() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.box = "vertical"
      ) +
      ylab("Proportion of mangroves") +
      xlab("Area-weighted resilience") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 0.07)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 100.1))

    dir.create(paste0(
      "Figures/Country/05a_overlap_WDPA/",
      split_group, "_", targets, "/RDS/"
    ), recursive = TRUE)

    saveRDS(plot_density, paste0(
      "Figures/Country/05a_overlap_WDPA/",
      split_group, "_", targets, "/RDS/overlap_WDPA_resilience_",
      CC_direction, "_", prct, ".rds"
    ))

    openxlsx::write.xlsx(plot_data, paste0(
      "Figures/Country/05a_overlap_WDPA/",
      split_group, "_", targets, "/overlap_WDPA_resilience_",
      CC_direction, "_", prct, ".xlsx"
    ))

    return(plot_density)
  })

  # Combine and save the plots
  (plot_density[[2]] +
    ggtitle("Landward")) /
    (plot_density[[3]] +
      ggtitle("Seaward")) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      plot.tag = element_text(size = 12, face = "bold"),
      title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    ) &
    plot_annotation(tag_levels = "a")

  # Save the combined plot
  ggsave(
    paste0(
      "Figures/Country/05a_overlap_WDPA/",
      split_group, "_", targets, "/overlap_WDPA_resilience_", prct, ".pdf"
    ),
    width = 18, height = 18, units = "cm", dpi = 300
  )
})

# Clean up
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() # free up memrory and report the memory usage.
.rs.restartR()
