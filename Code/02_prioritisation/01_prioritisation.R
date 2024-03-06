#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr)

PUs <- readRDS("Results/RDS/crop/PUs_05_mangroves_cc_IUCN_split_by_MEOW_and_biotyp_crop_priority.rds")
PUs_features_targets <- readRDS("Results/RDS/crop/PUs_05_features_targets.rds")

prioritizr_problem <- problem(PUs, PUs_features_targets$feature, cost_column = "area_m2") %>% 
  add_relative_targets(PUs_features_targets$targets) %>% 
  add_locked_in_constraints(PUs$priority) %>% 
  add_min_set_objective() %>% 
  add_gurobi_solver()

solution <- solve(prioritizr_problem, force = TRUE)

plot(solution[, "Prob_gain_stability_landward"])
