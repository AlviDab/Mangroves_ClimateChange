#Author: Alvise Dabalà
#Date: 14/06/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, spatstat, collapse, modelr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

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
        bind_rows()
    })
  })

new_area_cr <- area_cr[[1]][[1]] %>%
  mutate(prct_increase_area = (Area_mangrove_km2 - Area_mangrove_km2[1])/Area_mangrove_km2,
         prct_increase_mean_resilience = (mean_resilience - mean_resilience[1])/mean_resilience,
         ratio_prct_increase = prct_increase_mean_resilience/prct_increase_area*100)

log_curve <- lm(mean_resilience~log(Area_mangrove_km2), data = new_area_cr)

model <- new_area_cr %>%
  data_grid(Area_mangrove_km2 = seq_range(Area_mangrove_km2, n = 100, expand = 0.1)) %>%
  mutate(mean_resilience = predict(log_curve, model))

ggplot(data = new_area_cr, aes(x = Area_mangrove_km2,
                               y = mean_resilience)) +
  geom_point() +
  geom_line(data = model, aes(x = Area_mangrove_km2,
                              y = mean_resilience)) +
  theme_bw()