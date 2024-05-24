#Author: Alvise Dabalà
#Date: 08/04/2024

pacman::p_load(tidyverse, sf, MoMAColors, purrr, furrr, parallel, openxlsx)

source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_countries.R")

CC_direction <- "mean"

PUs <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds") %>%
  f_int_countries() %>%
  mutate(country = case_when(country == "France" ~ "French Guiana",
                             .default = country) %>%
           gsub(" ", "_", .))

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

plot_layer <- map(c("country_and_biotyp", "biotyp"), function(split_group) {

  solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                             split_group,"/solution_prioritisation.rds"))

  future_map(seq(0.05, 0.3, by = 0.05),
             .options = furrr_options(seed = TRUE),
             function(prct) {

               solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                             split_group, "/",
                                             CC_direction, "/solution_",
                                             as.character(prct), "_", CC_direction, ".rds"))

               name_split_group <- ifelse(split_group == "biotyp",
                                          "Split by biophysical typology",
                                          "Split by biophysical typology and marine ecoregion")

               plot_layer <- solution_cc %>%
                 left_join(PUs %>%
                             as_tibble() %>%
                             dplyr::select(country, ID), by = "ID") %>%
                 group_by(country, solution_1) %>%
                 summarise(tot_area = sum(area_km2),
                           cc_exp = weighted.mean(Prob_gain_stability_mean, area_km2)) %>%
                 pivot_wider(names_from = "solution_1", values_from = c("tot_area", "cc_exp")) %>%
                 group_by(country) %>%
                 summarise(across(ends_with(c("0", "1")), ~sum(., na.rm = TRUE))) %>%
                 mutate(perc_sel_area = tot_area_1/(tot_area_1 + tot_area_0)*100,
                        res_var = round((cc_exp_1 - cc_exp_0), 3)) %>%
                 f_int_continents() %>%
                 mutate(continent = case_when(grepl("America", continent) ~ "America",
                                              continent == "Europe" ~ "America",
                                              .default = continent)) %>%
                 mutate(prct = prct,
                        split_group = name_split_group) %>%
                 mutate(log_res_var = case_when( #make a log transformation symmetrical respect 0
                   res_var > 0 ~ log10(res_var),
                   res_var < 0 ~ -log10(abs(res_var)),
                   .default = 0
                 ))
             })
}) %>%
  bind_rows()

zero_high_selection <- plot_layer %>%
  st_drop_geometry() %>%
  mutate(zero_area = (perc_sel_area == 0),
         mid_selection = (perc_sel_area >= 50),
         high_selection = (perc_sel_area >= 75)) %>%
  group_by(prct, split_group) %>%
  summarise(num_zero_area = sum(zero_area),
            num_mid_selection = sum(mid_selection),
            num_high_selection = sum(high_selection))

write.xlsx(zero_high_selection, paste0("Figures/Country/08_plot_area_resilience/",
                                       CC_direction,
                                       "/zero_or_high_selection_areas.xlsx"))

sd_prct_sel_area <- plot_layer %>%
  st_drop_geometry() %>%
  group_by(prct, split_group) %>%
  summarise(perc_sel_area_sd = sd(perc_sel_area))

write.xlsx(sd_prct_sel_area, paste0("Figures/Country/08_plot_area_resilience/",
                                    CC_direction,
                                    "/percentage_area_selected_standard_deviation.xlsx"))

plot_layer_mean <- plot_layer %>%
  group_by(prct, split_group) %>%
  summarise(w_mean_res_var = weighted.mean(log_res_var, tot_area_1),
            perc_selected_area = sum(tot_area_1)/sum(tot_area_1 + tot_area_0)*100) #Weighted mean of the resilience using the area of mangrove selected

plot <- ggplot(data = plot_layer,
               aes(x = log_res_var, y = perc_sel_area)) +
  geom_point(aes(fill = continent, size = tot_area_1), alpha = 0.8,
             shape = 21,
             stroke = NA) +
  scale_fill_moma_d("Smith", name = "") +
  geom_vline(data = plot_layer_mean, aes(xintercept = w_mean_res_var),
             linetype = 2, linewidth = 0.4) +
  geom_hline(data = plot_layer_mean, aes(yintercept = perc_selected_area),
             linetype = 2, linewidth = 0.4) +
  ylab("Percentage of the area selected") +
  xlab(expression("log"[10]*"(resilience variation)")) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_text(size = 11, face = "bold"),
        panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.box = 'vertical') +
  scale_size_continuous(name = "Mangrove area selected in the climate-smart solution (km²)") +
  guides(fill = guide_legend(override.aes = list(size = 3)),
         size = guide_legend(title.position = "top")) +
  facet_grid(prct ~ split_group)

dir.create(paste0("Figures/Country/08_plot_area_resilience/mean/RDS"), recursive = TRUE)

ggsave(plot = plot, paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/area_resilience_",
                           CC_direction, "_by_ecoregion.pdf"),
       dpi = 300, width = 18, height = 25, units = "cm")

saveRDS(plot, paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/RDS/area_resilience_",
                     CC_direction, "_by_ecoregion.rds"))

write.xlsx(plot_layer %>%
             st_drop_geometry(), paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/area_resilience_",
                                        CC_direction, "_by_ecoregion.xlsx"))

write.xlsx(plot_layer_mean %>%
             st_drop_geometry(), paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/area_resilience_",
                                        CC_direction, "_by_ecoregion_mean.xlsx"))

#Description of the figures
writeLines("Comparison of the different outcomes of the climate-smart prioritisations against the climate-naive prioritisation.

Each point represent a different ecoregion/province. The size of the point is the km² of mangrove area selected in the climate-smart solution.

The y-axis represent the percentage of mangrove area selected by the climate-smart prioritisation.

The x-axis represent the difference in the climate resilience between the climate-smart and climate-naive. The value of resilience for each ecoregion/province is the area weighted mean of the resilience of the mangrove areas selected in the prioritisation.
The x-axis values are in logarithmic scale. For the negative values of resilience, we scaled the absolute value to logarithmic and then inverted the resulting value.

In the boxes on the right side of the figures are reported the thresholds used for the selection of the climate-priority areas of the climate-smart prioritisation.

The vertical dashed line show the area weighted mean value of resilience variation between the climate-smart and the climate-naive prioritisation.

The points that show a percentage of area selected equal to zero present different resilience variation values. These are just the opposite of the resilience value of the areas selected in the climate-naive prioritisation as there is no selection of areas in that ecoregion/province in the climate-smart prioritisation."
           , paste0("Figures/Country/08_plot_area_resilience/", CC_direction, "/info.txt"))

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()