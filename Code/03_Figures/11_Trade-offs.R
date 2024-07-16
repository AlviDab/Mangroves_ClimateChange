#Author: Alvise Dabalà
#Date: 14/06/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, spatstat, collapse)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

area_cr <- map(c(#"landward", "seaward",
  "mean"), function(CC_direction) {

    map(c("country_and_biotyp", "biotyp"), function(split_group) {

      future_map(seq(0, 0.3, by = 0.05),
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
                     summarise(median_resilience = spatstat.geom::weighted.median(Prob_gain_stability_mean,
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

lm_area_cr <- lm(prct_increase_area ~ prct_increase_mean_resilience, data = new_area_cr)

new_area_cr <- area_cr[[1]][[1]] %>%
  mutate(prct_increase_area = (Area_mangrove_km2 - Area_mangrove_km2[1])/Area_mangrove_km2,
         prct_increase_mean_resilience = (mean_resilience - mean_resilience[1])/mean_resilience,
         ratio_prct_increase = prct_increase_mean_resilience/prct_increase_area*100)

ggplot(data = new_area_cr, aes(x = prct_increase_area,
                               y = prct_increase_mean_resilience)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = new_area_cr, aes(x = Area_mangrove_km2,
                               y = mean_resilience)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = new_area_cr, aes(x = Area_mangrove_km2,
                               y = median_resilience)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = new_area_cr, aes(x = Area_mangrove_km2,
                               y = area_climate_resilient_75)) +
  geom_point() +
  geom_smooth(se=FALSE, method = "glm", formula= y ~ poly(x,2))
