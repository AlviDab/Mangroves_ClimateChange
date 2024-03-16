#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr)

prct <- 0.05
CC_direction <- "landward"

PUs <- readRDS(paste0("Results/RDS/prioritisation_input/PUs_05_mangroves_cc_IUCN_split_by_MEOW_and_biotyp_priority_",
       prct, "_", CC_direction, ".rds"))

PUs_features_split_targets <- readRDS("Results/RDS/PUs_05_features_split_targets.rds")

#Incremental priority area percentage 5% increment from 5% to 30%
prioritizr_problem <- problem(PUs,
                              PUs_features_split_targets$feature,
                              cost_column = "area_km2") %>%
  add_relative_targets(PUs_features_split_targets$targets) %>% #Change the targets
  add_locked_in_constraints(PUs$priority) %>%
  add_min_set_objective() %>%
  add_gurobi_solver()

solution <- solve(prioritizr_problem)

plot(solution[, "solution_1"])

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
