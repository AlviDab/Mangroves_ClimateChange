#Author: Alvise Dabalà
#Date: 21/02/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

PUs_start <- readRDS("Results/RDS/PUs_04_mangroves_cc_IUCN_split_by_MEOW_and_biotyp.rds")

source("Code/Functions/f_find_priority.r")

dir.create("Results/RDS/prioritisation_input/")

#add mean probability of gain stability
PUs_start <- PUs_start %>%
  mutate(Prob_gain_stability_mean =
           (Prob_gain_stability_landward + Prob_gain_stability_seaward)/2
  )

#remove eventual NAs using nearest neighborhood

#Add using nearest neighborhood the missing values
source("Code/Functions/f_remove_NAs_nearestneighborhood.R")

PUs <- PUs_start %>%
  fNN_NAs("Prob_gain_stability_landward") %>%
  fNN_NAs("Prob_gain_stability_seaward") %>%
  fNN_NAs("Prob_gain_stability_mean")

diffdf::diffdf(tibble(PUs_start),
               tibble(PUs))

#ncores <- detectCores() - 2

#plan(multisession, workers = ncores)

map(seq(0.05, 0.3, by = 0.05),
    #.options = furrr_options(seed = TRUE),
    function(prct) {
      map(c(#"landward", "seaward",
        "mean"),
        #.options = furrr_options(seed = TRUE),
        function(CC_direction) {
          PUs_CC <- f_find_priority(PUs,
                                    paste0("Prob_gain_stability_", CC_direction),
                                    prct)

          saveRDS(PUs_CC,
                  paste0("Results/RDS/prioritisation_input/PUs_05_mangroves_cc_IUCN_split_by_MEOW_and_biotyp_priority_",
                         prct, "_", CC_direction, ".rds")
          )
        })
    })

#plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
