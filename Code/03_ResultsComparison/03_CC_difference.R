#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf)

prct <- 0.3
CC_direction <- "mean"

solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/solution_prioritisation.rds"))

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds"))

source("Code/Functions/f_create_kdplot.r")

#mean climate risk climate-naïve
solution %>%
  as_tibble() %>%
  group_by(solution_1) %>%
  summarise(mean_exposure = weighted.mean(Prob_gain_stability_mean, area_km2))

selected_cn <- solution %>%
  filter(solution_1 == 1) %>%
  as_tibble()

kd_plot_cn <- ggplot() +
  geom_density(data = selected_cn, aes(x = Prob_gain_stability_mean)) +
  ylim(c(0, 0.03)) +
  theme_bw()

#mean climate risk climate-smart
solution_cc %>%
  as_tibble() %>%
  group_by(solution_1) %>%
  summarise(mean_exposure = weighted.mean(Prob_gain_stability_mean, area_km2))

selected_cs <- solution_cc %>%
  filter(solution_1 == 1) %>%
  as_tibble()

kd_plot_cs <- ggplot() +
  geom_density(data = selected_cs, aes(x = Prob_gain_stability_mean)) +
  ylim(c(0, 0.03)) +
  theme_bw()

