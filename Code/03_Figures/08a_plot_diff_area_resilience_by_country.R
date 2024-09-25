#Author: Alvise Dabalà
#Date: 11/07/2024

pacman::p_load(tidyverse, sf, MoMAColors, purrr, furrr, parallel, openxlsx, ggrepel, ggpubr)

source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_countries.R")

PUs <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds") %>%
  f_int_countries() %>%
  mutate(country = case_when(country == "France" ~ "French Guiana",
                             .default = country) %>%
           gsub(" ", "_", .))

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

# future_map(seq(0.05, 0.3, by = 0.05),
#            .options = furrr_options(seed = TRUE),
#            function(prct) {
prct <- 0.3

map(c("mean", "landward", "seaward"), function(CC_direction) {

  plot_layer <- map(c("country_and_biotyp", "biotyp"), function(split_group) {

    solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                               split_group,"/solution_prioritisation.rds"))

    solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                  split_group, "/",
                                  CC_direction, "/solution_",
                                  as.character(prct), "_", CC_direction, ".rds"))

    name_split_group <- ifelse(split_group == "biotyp",
                               "Global scale",
                               "Country scale")

    comp_solution <- solution_cc %>%
      dplyr::select(!starts_with("Sp")) %>%
      mutate(solution_noCC = solution$solution_1) %>%
      rename(solution_CC = solution_1) %>%
      left_join(PUs %>%
                  as_tibble() %>%
                  dplyr::select(country, ID), by = "ID")

    #Area by country
    all_PUs <- comp_solution %>%
      group_by(country) %>%
      summarise(tot_mangrove_area = sum(MangroveArea_km2),
                tot_mean_resilience = weighted.mean(Prob_gain_stability_mean,
                                                    MangroveArea_km2))

    sel_PUs_CC <- comp_solution %>%
      st_drop_geometry() %>%
      filter(solution_CC == 1) %>%
      group_by(country) %>%
      summarise(CC_mangrove_area = sum(MangroveArea_km2),
                CC_mean_resilience = weighted.mean(Prob_gain_stability_mean,
                                                   MangroveArea_km2))

    sel_PUs_noCC <- comp_solution %>%
      st_drop_geometry() %>%
      filter(solution_noCC == 1) %>%
      group_by(country) %>%
      summarise(noCC_mangrove_area = sum(MangroveArea_km2),
                noCC_mean_resilience = weighted.mean(Prob_gain_stability_mean,
                                                     MangroveArea_km2))

    #Join results
    joined_results <- all_PUs %>%
      left_join(sel_PUs_CC, by = "country") %>%
      left_join(sel_PUs_noCC, by = "country") %>%
      filter(!is.na(CC_mangrove_area) | !is.na(noCC_mangrove_area)) %>% #Remove countries that are not selected in both
      mutate(
        across(!geometry, ~replace_na(.x, 0))
      ) %>%
      mutate(prct = prct,
             split_group = name_split_group) %>%
      mutate(diff_perc_area_CC_noCC = ((CC_mangrove_area/tot_mangrove_area) -
                                         (noCC_mangrove_area/tot_mangrove_area))*100,
             diff_perc_resilience_CC_noCC = ((CC_mean_resilience - noCC_mean_resilience)/
                                               noCC_mean_resilience)*100)%>%
      mutate(diff_perc_resilience_CC_noCC = case_when( #make a log transformation symmetrical respect 0
        diff_perc_resilience_CC_noCC > 0 ~ log10(diff_perc_resilience_CC_noCC + 1), #removing 1 so that the minimum is zero
        diff_perc_resilience_CC_noCC < 0 ~ -log10(abs(diff_perc_resilience_CC_noCC - 1)), #adding 1 so that the minimum is zero
        .default = 0
      )) %>%
      mutate(diff_perc_area_CC_noCC = case_when( #make a log transformation symmetrical respect 0
        diff_perc_area_CC_noCC > 0 ~ log10(diff_perc_area_CC_noCC + 1), #removing 1 so that the minimum is zero
        diff_perc_area_CC_noCC < 0 ~ -log10(abs(diff_perc_area_CC_noCC - 1)), #adding 1 so that the minimum is zero
        .default = 0
      )) %>%
      f_int_continents() %>%
      mutate(continent = case_when(grepl("America", continent) ~ "America",
                                   continent == "Europe" ~ "America",
                                   .default = continent)) %>%
      mutate(country = str_replace_all(country, "_", " "))
  }) %>%
    bind_rows()

  #remove infinite value and order the databases
  plot_layer_no_inf <- plot_layer %>%
    filter(is.finite(diff_perc_resilience_CC_noCC))

  plot <- ggplot(data = plot_layer_no_inf,
                 aes(x = diff_perc_resilience_CC_noCC, y = diff_perc_area_CC_noCC)) +
    geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed",
               alpha = 0.8, colour = "grey20",) +
    geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed",
               alpha = 0.8, colour = "grey20",) +
    geom_point(aes(fill = continent, size = CC_mangrove_area), alpha = 0.8,
               shape = 21,
               stroke = NA) +
    guides(fill = guide_legend(override.aes = list(size = 5))) +
    geom_text_repel(data = plot_layer_a,
                    aes(x = diff_perc_resilience_CC_noCC, y = diff_perc_area_CC_noCC, label = country),
                    hjust = 0, min.segment.length = 0, max.overlaps = 4,
                    force = 30,
                    max.iter = 5000, size = 3) +
    scale_fill_moma_d("Smith", name = "") +
    ylab(expression("log"[10]*"(percentage difference area selected)")) +
    xlab(expression("log"[10]*"(percentage increase in climate resilience)")) +
    theme_bw() +
    theme(legend.position = 'bottom',
          title = element_text(size = 11, face = "bold"),
          legend.title = element_text(size = 10, face = "bold"),
          panel.grid.major = element_line(colour = "transparent"),
          panel.background = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.title.position = "top",
          legend.box = 'vertical',
          legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold"),
          legend.spacing.y = unit(0, "cm")) +
    scale_size_continuous(name = "Mangrove area selected in the climate-smart solution (km²)",
                          breaks = c(0, 5000, 10000, 15000),
                          labels = c("0-5000", ">5000", ">10000", ">15000")) +
    scale_x_continuous(expand = c(0.02, 0.02), limits = c(-3, 3)) +
    # guides(fill = guide_legend(override.aes = list(size = 3)),
    #        size = guide_legend(title.position = "top")) +
    # facet_grid(prct ~ split_group)
    facet_wrap(~factor(split_group, levels = c("Global scale",
                                              "Country scale"))) +
    theme(strip.text.x = element_text(size = 11, face = 'bold'),
          axis.title = element_text(size = 10, face = "bold"))

  plot_legend <- ggpubr::get_legend(plot) %>%
    as_ggplot()

  dir.create(paste0("Figures/Country/08a_plot_diff_area_resilience/", CC_direction, "/RDS"), recursive = TRUE)

  ggsave(plot = plot, paste0("Figures/Country/08a_plot_diff_area_resilience/", CC_direction, "/diff_area_resilience_",
                             CC_direction, "_by_country_", prct, ".pdf"),
         dpi = 300, width = 18, height = 10, units = "cm")

  saveRDS(plot, paste0("Figures/Country/08a_plot_diff_area_resilience/", CC_direction, "/RDS/diff_area_resilience_",
                       CC_direction, "_by_country_", prct, ".rds"))

  saveRDS(plot_legend, paste0("Figures/Country/08a_plot_diff_area_resilience/", CC_direction, "/RDS/diff_area_resilience_",
                       CC_direction, "_by_country_", prct, "_legend.rds"))

  write.xlsx(plot_layer %>%
               st_drop_geometry(), paste0("Figures/Country/08a_plot_diff_area_resilience/", CC_direction, "/diff_area_resilience_",
                                          CC_direction, "_by_country_", prct, ".xlsx"))

})
# })

# #Description of the figures
# writeLines("Comparison of the different outcomes of the climate-smart prioritisations against the climate-naive prioritisation.
#
# Each point represent a different country/province. The size of the point is the km² of mangrove area selected in the climate-smart solution.
#
# The y-axis represent the percentage of mangrove area selected by the climate-smart prioritisation.
#
# The x-axis represent the difference in the climate resilience between the climate-smart and climate-naive. The value of resilience for each country/province is the area weighted mean of the resilience of the mangrove areas selected in the prioritisation.
# The x-axis values are in logarithmic scale. For the negative values of resilience, we scaled the absolute value to logarithmic and then inverted the resulting value.
#
# In the boxes on the right side of the figures are reported the thresholds used for the selection of the climate-priority areas of the climate-smart prioritisation.
#
# The vertical dashed line show the area weighted mean value of resilience variation between the climate-smart and the climate-naive prioritisation.
#
# The points that show a percentage of area selected equal to zero present different resilience variation values. These are just the opposite of the resilience value of the areas selected in the climate-naive prioritisation as there is no selection of areas in that country/province in the climate-smart prioritisation."
#            , paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/info.txt"))

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
#.rs.restartR()