#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05),
           .options = furrr_options(seed = TRUE),
           function(prct) {
             CC_direction <- "mean"

             PUs <- readRDS(paste0("Results/RDS/prioritisation_input/PUs_05_mangroves_cc_IUCN_split_by_MEOW_and_biotyp_priority_",
                                   prct, "_", CC_direction, ".rds"))

             PUs_features_split_targets <- readRDS("Results/RDS/PUs_05_features_split_targets.rds")

             prioritizr_problem <- problem(PUs,
                                           PUs_features_split_targets$feature,
                                           cost_column = "area_km2") %>%
               add_relative_targets(PUs_features_split_targets$targets) %>%
               add_locked_in_constraints(PUs$priority) %>%
               add_min_set_objective() %>%
               add_gurobi_solver()

             solution <- solve(prioritizr_problem)

             metrics <- prioritizr::eval_target_coverage_summary(prioritizr_problem, solution[, "solution_1"])

             dir.create(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                               CC_direction), recursive = T)

             saveRDS(solution, paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                      CC_direction, "/solution_",
                                      as.character(prct), "_", CC_direction, ".rds"))

             saveRDS(metrics, paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                     CC_direction, "/metrics_",
                                     as.character(prct), "_", CC_direction, ".rds"))
           })

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
