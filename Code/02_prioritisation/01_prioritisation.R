#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr)

PUs <- readRDS("Results/RDS/prioritisation_input/PUs_05_mangroves_cc_IUCN_split_by_MEOW_and_biotyp_priority_0.05_mean.rds")
PUs_features_split_targets <- readRDS("Results/RDS/PUs_05_features_split_targets.rds")

prioritizr_problem <- problem(PUs,
                              PUs_features_split_targets$feature,
                              cost_column = "area_km2") %>%
  add_relative_targets(PUs_features_split_targets$targets) %>%
  add_min_set_objective() %>%
  add_gurobi_solver()

solution <- solve(prioritizr_problem, force = TRUE)

dir.create("Results/RDS/prioritisation/01_prioritisation/", recursive = T)
saveRDS(solution, "Results/RDS/prioritisation/01_prioritisation/solution_prioritisation.rds")
