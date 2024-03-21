#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr)

map(c("MEOW_and_", ""), function(file_name) {

  PUs <- readRDS(paste0("Results/RDS/prioritisation_input/PUs_05_mangroves_cc_IUCN_split_by_",
                        file_name, "biotyp_priority_0.05_mean.rds"))

  PUs_features_split_targets <- readRDS(paste0("Results/RDS/PUs_05_features_split_targets_by_",
                                               file_name, "biotyp.rds"))

  new_file_name <- ifelse(file_name == "MEOW_and_", "MEOW_and_biotyp", "biotyp")

  prioritizr_problem <- problem(PUs,
                                PUs_features_split_targets$feature,
                                cost_column = "area_km2") %>%
    add_relative_targets(PUs_features_split_targets$targets) %>%
    add_min_set_objective() %>%
    add_gurobi_solver()

  solution <- solve(prioritizr_problem)

  metrics <- prioritizr::eval_target_coverage_summary(prioritizr_problem, solution[, "solution_1"])

  dir.create(paste0("Results/RDS/prioritisation/01_prioritisation/", new_file_name), recursive = T)

  saveRDS(solution, paste0("Results/RDS/prioritisation/01_prioritisation/",
                           new_file_name,
                           "/solution_prioritisation.rds"))

  saveRDS(metrics, paste0("Results/RDS/prioritisation/01_prioritisation/",
                          new_file_name,
                          "/metrics.rds"))
})