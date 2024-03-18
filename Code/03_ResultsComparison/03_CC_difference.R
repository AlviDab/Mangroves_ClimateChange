#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf)

prct <- 0.05
CC_direction <- "mean"

solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/solution_prioritisation.rds"))

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds"))

#mean climate risk climate-naïve
solution %>%
  as_tibble() %>%
  group_by(solution_1) %>%
  summarise(mean_exposure = weighted.mean(Prob_gain_stability_mean, area_km2))

#mean climate risk climate-smart
solution_cc %>%
  as_tibble() %>%
  group_by(solution_1) %>%
  summarise(mean_exposure = weighted.mean(Prob_gain_stability_mean, area_km2))
