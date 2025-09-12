#Author: Alvise Dabalà
#Date: 21/02/2024
#Description: This script selects the priority CPAs for mangrove restoration
#             based on the probability of gain stability under climate
#             change scenarios.Using the climate-priority area approach from
#             Buenafe et al. 2023 (https://doi.org/10.1002/eap.2852)

################################################################################

# load packages
pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

# change the maximum size of global variables for future package
options(future.globals.maxSize = 750*1024^2)

map(c("PUs_04_mangroves_cc_IUCN_split_by_country_and_biotyp",
      "PUs_04a_mangroves_cc_IUCN_split_by_biotyp"
      ), function(file_name) {

        PUs <- readRDS(paste0("Results/RDS/", file_name, ".rds"))

        split <- sub(".*_by", "by", file_name)
        new_file_name <- sub(".*_m", "m", file_name)

        features <- paste0("Results/RDS/PUs_05_features_split_targets_",
                           split,
                           ".rds") %>%
          readRDS()

        source("Code/Functions/f_find_priority.r")

        dir.create("Results/RDS/prioritisation_input/Country", recursive = TRUE)

        #add mean probability of gain stability
        PUs <- PUs %>%
          mutate(Prob_gain_stability_mean =
                   (Prob_gain_stability_landward + Prob_gain_stability_seaward)/2
          )

        #remove eventual NAs using nearest neighborhood
        source("Code/Functions/f_remove_NAs_nearestneighborhood.R")

        PUs <- PUs %>%
          fNN_NAs("Prob_gain_stability_landward") %>%
          fNN_NAs("Prob_gain_stability_seaward") %>%
          fNN_NAs("Prob_gain_stability_mean")

        map(seq(0.05, 1, by = 0.05),
            function(prct) {
              map(c("landward", "seaward",
                    "mean"
                ),
                function(CC_direction) {

                  # Find priority
                  PUs_CC <- f_find_priority(PUs,
                                            paste0("Prob_gain_stability_", CC_direction),
                                            prct,
                                            features)

                  saveRDS(PUs_CC,
                          paste0("Results/RDS/prioritisation_input/Country/PUs_05_",
                          new_file_name,
                          "_priority_",
                                 prct, "_", CC_direction, ".rds")
                  )
                })
            })
      })

# Clean up
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
