#Author: Alvise Dabalà
#Date: 20/02/2024
#Description: Prioritisation of mangroves for conservation without considering
#             climate change

################################################################################

# Load packages
pacman::p_load(tidyverse, sf, prioritizr)

map(c("targets_30", "targets_area"), function(targets) {
  map(c("country_and_", ""), function(file_name) {
    
  # Load data
    PUs <- readRDS(
      paste0(
        "Results/RDS/prioritisation_input/Country/PUs_05_mangroves_cc_IUCN_split_by_",
        file_name,
        "biotyp_priority_0.05_mean.rds"
      )
    )

    PUs_features_split_targets <- readRDS(
      paste0(
        "Results/RDS/PUs_05_features_split_targets_by_",
        file_name,
        "biotyp.rds"
      )
    )

    if(targets == "targets_30") {PUs_features_split_targets$targets <- 0.3}


    new_file_name <- ifelse(file_name == "country_and_",
                            paste0("country_and_biotyp_", targets),
                            paste0("biotyp_", targets))
    
    # Create prioritizr problem
    prioritizr_problem <- problem(PUs, PUs_features_split_targets$feature,
                                  cost_column = "area_km2") %>%
      add_relative_targets(PUs_features_split_targets$targets) %>%
      add_min_set_objective() %>%
      add_gurobi_solver(gap = 1 * 10 ^ -4)

    # Solve problem
    solution <- solve(prioritizr_problem)

    #replacement_score <- eval_replacement_importance(prioritizr_problem, solution[, "solution_1"])

    # Evaluate target coverage
    metrics <- prioritizr::eval_target_coverage_summary(prioritizr_problem,
                                                        solution[, "solution_1"])

    dir.create(
      paste0(
        "Results/resp_v1/RDS/prioritisation/Country/01_prioritisation/",
        new_file_name
      ),
      recursive = T
    )
    dir.create(
      paste0(
        "Results/resp_v1/gpkg/prioritisation/Country/01_prioritisation/",
        new_file_name
      ),
      recursive = T
    )

    saveRDS(
      solution,
      paste0(
        "Results/resp_v1/RDS/prioritisation/Country/01_prioritisation/",
        new_file_name,
        "/solution_prioritisation.rds"
      )
    )

    st_write(
      solution %>%
        dplyr::select(
          c(
            "solution_1",
            "Prob_gain_stability_mean",
            "Prob_gain_stability_seaward",
            "MangroveArea_km2"
          )
        ),
      paste0(
        "Results/resp_v1/gpkg/prioritisation/Country/01_prioritisation/",
        new_file_name,
        "/solution_prioritisation.gpkg"
      ),
      append = TRUE
    )

    # saveRDS(replacement_score, paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
    #                                   new_file_name,
    #                                   "/replacement_score.rds"))

    saveRDS(
      metrics,
      paste0(
        "Results/resp_v1/RDS/prioritisation/Country/01_prioritisation/",
        new_file_name,
        "/metrics.rds"
      )
    )
  })
})

# Clean up R environment
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
.rs.restartR()