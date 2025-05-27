#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, prioritizr, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

map(c("targets_30", "targets_area"), function(targets) {
  future_map(seq(0.05, 1, by = 0.05),
             .options = furrr_options(seed = TRUE),
             function(prct) {

               map(c("landward", "seaward",
                     "mean"
               ), function(CC_direction) {

                 map(c("country_and_", ""), function(file_name) {

                   PUs <- readRDS(paste0("Results/RDS/prioritisation_input/Country/PUs_05_mangroves_cc_IUCN_split_by_",
                                         file_name, "biotyp_priority_",
                                         prct, "_", CC_direction, ".rds"))

                   PUs_features_split_targets <- readRDS(paste0("Results/RDS/PUs_05_features_split_targets_by_",
                                                                file_name, "biotyp.rds"))

                   new_file_name <- ifelse(file_name == "country_and_",
                                           paste0("country_and_biotyp_", targets),
                                           paste0("biotyp_", targets))

                   if(targets == "targets_30") {PUs_features_split_targets$targets <- 0.3}

                   prioritizr_problem <- problem(PUs,
                                                 PUs_features_split_targets$feature,
                                                 cost_column = "area_km2") %>%
                     add_relative_targets(PUs_features_split_targets$targets) %>%
                     add_locked_in_constraints(PUs$priority) %>%
                     add_min_set_objective() %>%
                     add_gurobi_solver(gap = 1*10^-4)

                   solution <- solve(prioritizr_problem)

                   # replacement_score <- eval_replacement_importance(prioritizr_problem,
                   #                                                  solution[, "solution_1"])

                   metrics <- prioritizr::eval_target_coverage_summary(prioritizr_problem,
                                                                       solution[, "solution_1"])

                   dir.create(paste0("Results/resp_v1/RDS/prioritisation/Country/02_prioritisation_CC/",
                                     new_file_name, "/",
                                     CC_direction), recursive = T)

                   dir.create(paste0("Results/resp_v1/gpkg/prioritisation/Country/02_prioritisation_CC/",
                                     new_file_name, "/",
                                     CC_direction), recursive = T)

                   saveRDS(solution, paste0("Results/resp_v1/RDS/prioritisation/Country/02_prioritisation_CC/",
                                            new_file_name, "/",
                                            CC_direction, "/solution_",
                                            as.character(prct), "_", CC_direction, ".rds"))

                   st_write(solution %>%
                              dplyr::select(geometry,
                                            solution_1), paste0("Results/resp_v1/gpkg/prioritisation/Country/02_prioritisation_CC/",
                                                                new_file_name, "/",
                                                                CC_direction, "/solution_",
                                                                as.character(prct), "_", CC_direction, ".gpkg"),
                            append = TRUE)

                   # saveRDS(replacement_score, paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                   #                                   new_file_name, "/",
                   #                                   CC_direction, "/replacement_score_",
                   #                                   as.character(prct), "_", CC_direction, ".rds"))

                   saveRDS(metrics, paste0("Results/resp_v1/RDS/prioritisation/Country/02_prioritisation_CC/",
                                           new_file_name, "/",
                                           CC_direction, "/metrics_",
                                           as.character(prct), "_", CC_direction, ".rds"))
                 })
               })
             })
})

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
.rs.restartR()
