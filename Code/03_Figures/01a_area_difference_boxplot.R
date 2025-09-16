# Author: Alvise Dabalà
# Date: 02/10/2024
# Description: Boxplot to compare the amount of area selected in climate-smart vs
#              climate-naïve prioritisation showing mean value and value by country

################################################################################

# Load packages
pacman::p_load(tidyverse, sf, parallel, furrr, purrr, openxlsx, ggeffects, ggstats, ggrepel, ggh4x)

# Load functions
source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_countries.r")

# Load PUs
PUs <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds") %>%
  f_int_countries()

# Select the parameters
prct <- 0.3
targets <- "targets_area"
# CC_direction <- "mean"

# ncores <- detectCores() - 2
#
# plan(multisession, workers = ncores)

# future_map(seq(0.05, 1, by = 0.05),
#            function(prct) {
#

# Map over the CC directions and split groups
map(c(
  "landward", "seaward",
  "mean"
), function(CC_direction) {
  col_name <- paste0("Prob_gain_stability_", CC_direction)

  selected <- map(c("country_and_biotyp", "biotyp"), function(split_group) {
    # load solutions
    solution <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/01_prioritisation/",
      split_group, "_", targets, "/solution_prioritisation.rds"
    ))

    solution_cc <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group, "_", targets, "/",
      CC_direction, "/solution_",
      as.character(prct), "_", CC_direction, ".rds"
    ))

    # define type indicator
    type_indicator <- ifelse(split_group == "country_and_biotyp",
      "country-scale",
      "global-scale"
    )

    # climate-naïve solution
    selected_cn <- solution %>%
      st_drop_geometry() %>%
      filter(solution_1 == 1) %>%
      mutate(
        type = paste("Climate-naïve", str_to_sentence(type_indicator)),
        climate = "Climate-naïve",
        scale = str_to_sentence(type_indicator)
      ) %>%
      as_tibble()

    # climate-smart solution
    selected_cs <- solution_cc %>%
      st_drop_geometry() %>%
      filter(solution_1 == 1) %>%
      mutate(
        type = paste("Climate-smart", str_to_sentence(type_indicator)),
        climate = "Climate-smart",
        scale = str_to_sentence(type_indicator)
      ) %>%
      as_tibble()

    # Bind the selected data
    selected <- rbind(selected_cn, selected_cs) %>%
      mutate(split_group = split_group)
  }) %>%
    bind_rows()

  # Summarise the area selected at country level
  selected_country <- selected %>%
    left_join(
      PUs %>%
        dplyr::select(c("country", "cellID")),
      by = "cellID"
    ) %>%
    group_by(country, type) %>%
    summarise(
      area_selected = sum(area_km2),
      climate = first(climate),
      scale = first(scale)
    ) %>%
    mutate(country = str_replace_all(country, "_", " "))

  # Total area by country
  PUs_country <- PUs %>%
    st_drop_geometry() %>%
    group_by(country) %>%
    summarise(tot_area = sum(area_km2)) %>%
    mutate(country = str_replace_all(country, "_", " "))

  # Calculate the percentage of area selected by country
  selected_country <- selected_country %>%
    ungroup() %>%
    complete(country, type,
      fill = list(area_selected = 0)
    ) %>%
    left_join(PUs_country,
      by = "country"
    ) %>%
    mutate(prct_area_selected = area_selected / tot_area * 100)

  # Summary of the area of mangroves in planning units selected by the prioritisations by type of prioritisation
  selected_summary <- selected_country %>%
    group_by(type) %>%
    summarise(
      tot_area = sum(tot_area),
      area_selected = sum(area_selected),
      mean_area = area_selected / tot_area * 100,
    )

  # Calculate the difference in area between the approaches
  selected_country_difference <- selected_country %>%
    dplyr::select(country, type, prct_area_selected) %>%
    pivot_wider(names_from = type, values_from = prct_area_selected) %>%
    mutate(
      diff_global_scale = `Climate-naïve Global-scale` - `Climate-smart Global-scale`,
      diff_country_scale = `Climate-naïve Country-scale` - `Climate-smart Country-scale`,
      diff_climate_smart = `Climate-smart Country-scale` - `Climate-smart Global-scale`
    )

  # Select countries to label
  countries_ggrepel_global_scale <- selected_country_difference %>%
    select(country, diff_global_scale) %>%
    slice_max(diff_global_scale, n = 3, with_ties = FALSE) %>%
    add_row(selected_country_difference %>%
      filter(`Climate-smart Global-scale` < 60 & `Climate-smart Global-scale` > 40 |
        `Climate-smart Global-scale` > -60 & `Climate-smart Global-scale` < -40) %>%
      select(country, diff_global_scale) %>%
      slice_min(diff_global_scale, n = 3, with_ties = FALSE)) %>%
    add_row(selected_country_difference %>%
      select(country, diff_global_scale) %>%
      slice_min(diff_global_scale, n = 3, with_ties = FALSE)) %>%
    select(country) %>%
    unlist() %>%
    c()

  countries_ggrepel_country_scale <- selected_country_difference %>%
    select(country, diff_country_scale) %>%
    slice_max(diff_country_scale, n = 3, with_ties = FALSE) %>%
    add_row(selected_country_difference %>%
      filter(`Climate-smart Country-scale` < 60 & `Climate-smart Country-scale` > 40 |
        `Climate-smart Country-scale` > -60 & `Climate-smart Country-scale` < -40) %>%
      select(country, diff_country_scale) %>%
      slice_min(diff_country_scale, n = 3, with_ties = FALSE)) %>%
    add_row(selected_country_difference %>%
      select(country, diff_country_scale) %>%
      slice_min(diff_country_scale, n = 3, with_ties = FALSE)) %>%
    select(country) %>%
    unlist() %>%
    c()

  countries_ggrepel_climate_smart <- selected_country_difference %>%
    select(country, diff_climate_smart) %>%
    slice_max(diff_climate_smart, n = 3, with_ties = FALSE) %>%
    add_row(selected_country_difference %>%
      filter(`Climate-smart Global-scale` < 60 & `Climate-smart Global-scale` > 40 |
        `Climate-smart Global-scale` > -60 & `Climate-smart Global-scale` < -40) %>%
      select(country, diff_climate_smart) %>%
      slice_min(diff_climate_smart, n = 3, with_ties = FALSE)) %>%
    add_row(selected_country_difference %>%
      select(country, diff_climate_smart) %>%
      slice_min(diff_climate_smart, n = 3, with_ties = FALSE)) %>%
    select(country) %>%
    unlist() %>%
    c()

  # position jitter
  pos <- position_jitter(seed = 1, width = 0.2) # position jitter

  # order of the levels in the x axis
  level_order <- c(
    "Climate-naïve Country-scale", "Climate-naïve Global-scale",
    "Climate-smart Country-scale", "Climate-smart Global-scale"
  )

  # Select plot colours and shapes
  map(c(
    # "all_comparisons",
    "Global-scale",
    "Country-scale",
    "Climate-smart"
  ), function(group_name) {
    if (group_name == "all_comparisons") {
      countries_ggrepel <- countries_ggrepel_climate_smart

      comparison <- selected_country
      comparison_summary <- selected_summary

      colours_plot <- c("#B80000", "#F2AC6B", "#003049", "#4B86AA")
      shapes_plot <- c(17, 19, 17, 19)
    } else {
      if (group_name == "Global-scale") {
        countries_ggrepel <- countries_ggrepel_global_scale

        colours_plot <- c("#F2AC6B", "#4B86AA")
        shapes_plot <- c(19, 19)
      } else if (group_name == "Country-scale") {
        countries_ggrepel <- countries_ggrepel_country_scale

        colours_plot <- c("#B80000", "#003049")
        shapes_plot <- c(17, 17)
      } else {
        countries_ggrepel <- countries_ggrepel_climate_smart

        colours_plot <- c("#003049", "#4B86AA")
        shapes_plot <- c(17, 19)
      }

      comparison <- selected_country %>%
        filter(str_detect(type, group_name))

      comparison_summary <- selected_summary %>%
        filter(str_detect(type, group_name))
    }

    # whether to invert the order of the x axis labels
    invert <- ifelse(group_name == "Global-scale", FALSE, TRUE)

    # Create the plot
    plot_comparison <- ggplot() +
      # geom_point(data = selected_summary, aes(x = type, y = weighted_mean_exposure),
      #            size = 4) +
      geom_errorbar(
        data = comparison_summary, aes(
          x = factor(type, level = level_order), y = mean_area,
          ymin = mean_area,
          ymax = mean_area,
          colour = factor(type, level = level_order)
        ),
        width = 0.6,
        linewidth = 1.2
      ) +
      geom_point(
        data = comparison, aes(
          y = prct_area_selected, x = factor(type, level = level_order),
          colour = factor(type, level = level_order),
          shape = factor(type, level = level_order)
        ),
        position = pos,
        size = 1.2, alpha = 0.6
      ) +
      geom_text_repel(
        data = comparison,
        aes(
          x = factor(type, level = level_order), y = prct_area_selected,
          label = ifelse(country %in% countries_ggrepel,
            country,
            ""
          )
        ),
        hjust = 0, min.segment.length = 0, max.overlaps = 30,
        force = 10,
        max.iter = 5000, size = 2.5,
        position = pos
      ) +
      scale_fill_manual(
        values = colours_plot,
        name = ""
      ) +
      scale_colour_manual(
        values = colours_plot,
        name = ""
      ) +
      scale_shape_manual(values = shapes_plot) +
      guides(
        fill = guide_legend(nrow = 2, byrow = TRUE),
        x = ggh4x::guide_axis_nested(delim = " ", inv = invert)
      ) +
      scale_y_continuous(limits = c(-5, 105), expand = c(0, 0)) +
      ylab(expression("Mangrove area selected (%)")) +
      xlab("") +
      theme_bw(base_size = 10) +
      theme(
        panel.background = element_blank(),
        text = element_text(size = 7),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        legend.key.size = unit(0.3, "cm"),
        plot.margin = margin(r = 0.5, unit = "cm"),
        axis.title.x = element_text(size = 12, face = "bold"),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 9)
      )

    # Save the plot
    dir.create(paste0("Figures/Country/01a_area/RDS"), recursive = TRUE)

    ggsave(
      plot = plot_comparison, paste0(
        "Figures/Country/01a_area/boxplot_area_",
        CC_direction, "_", prct, "_", group_name, ".jpg"
      ),
      dpi = 300, width = 18, height = 8, units = "cm"
    )

    ggsave(
      plot = plot_comparison, paste0(
        "Figures/Country/01a_area/boxplot_area_",
        CC_direction, "_", prct, "_", group_name, ".pdf"
      ),
      dpi = 300, width = 18, height = 8, units = "cm"
    )

    saveRDS(plot_comparison, paste0(
      "Figures/Country/01a_area/RDS/boxplot_area_",
      CC_direction, "_", prct, "_", group_name, ".rds"
    ))
  })
})
# })

rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() # free up memrory and report the memory usage.
