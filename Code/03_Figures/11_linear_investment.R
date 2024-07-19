#Author: Alvise Dabalà
#Date: 14/06/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, spatstat, collapse, modelr)

ncores <- detectCores() - 2

# plan(multisession, workers = ncores)

area_cr <- map(c(#"landward", "seaward",
  "mean"), function(CC_direction) {

    map(c("country_and_biotyp",
          "biotyp"), function(split_group) {

            future_map(seq(0, 1, by = 0.05),
                       function(prct) {
                         if(prct == 0) {
                           solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                      split_group,"/solution_prioritisation.rds"))
                         } else {

                           solution <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                      split_group, "/",
                                                      CC_direction, "/solution_",
                                                      as.character(prct), "_", CC_direction, ".rds"))

                         }

                         area <- solution %>%
                           st_drop_geometry() %>%
                           filter(solution_1 == 1) %>%
                           summarise(Area_mangrove_km2 = sum(MangroveArea_km2))

                         #Median resilience
                         climate_resilient_median_mean <- solution %>%
                           st_drop_geometry() %>%
                           filter(solution_1 == 1) %>%
                           summarise(median_resilience = spatstat.univar::weighted.median(Prob_gain_stability_mean,
                                                                                          MangroveArea_km2),
                                     mean_resilience = weighted.mean(Prob_gain_stability_mean,
                                                                     MangroveArea_km2))
                         # mode_resilience = collapse::fmode(Prob_gain_stability_mean,
                         #                                   MangroveArea_km2))

                         #Filter mangroves with resilience >= 75
                         climate_resilient_75 <- solution %>%
                           st_drop_geometry() %>%
                           filter(solution_1 == 1,
                                  Prob_gain_stability_mean >= 75) %>%
                           summarise(area_climate_resilient_75 = sum(MangroveArea_km2))

                         area_cr <- area %>%
                           cbind(climate_resilient_median_mean) %>%
                           cbind(climate_resilient_75) %>%
                           add_column(threshold = prct)
                       }) %>%
              bind_rows() %>%
              mutate(type = split_group) %>%
              mutate(prct_increase_area = (Area_mangrove_km2 - Area_mangrove_km2[1])/Area_mangrove_km2,
                     prct_increase_mean_resilience = (mean_resilience - mean_resilience[1])/mean_resilience,
                     ratio_prct_increase = prct_increase_mean_resilience/prct_increase_area*100)
          })
  }) %>%
  bind_rows()

ggplot(data = area_cr, aes(x = prct_increase_area*100,
                               y = prct_increase_mean_resilience*100)) +
  geom_point(aes(colour = type), alpha = 0.7, size = 2) +
  #geom_line(aes(colour = type), linetype = "dotted", linewidth = 1) +
  scale_colour_manual(values = c('#2a9d8f', '#F4A261'),
                      labels = c('Global scale',
                                 'Country scale'),
                      guide = guide_legend()) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(colour = "grey90", size = 0.1),
    panel.grid.minor = element_line(colour = "grey90", size = 0.05),
    legend.position = "top"
  ) +
  ylab("Percentage increase in resilience (%)") +
  xlab("Percentage increase in area (%)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(colour = "grey20",
                                    face = "bold"),
        axis.title.y = element_text(colour = "grey20",
                                    face = "bold")) +
  scale_x_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1)))
