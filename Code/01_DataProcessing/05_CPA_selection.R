#Author: Alvise Dabalà
#Date: 21/02/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

PUs <- readRDS("Results/RDS/PUs_04_mangroves_cc_IUCN_split_by_MEOW_and_biotyp.rds")

source("Code/Functions/f_find_priority.r")

dir.create("Results/RDS/prioritisation_input/")

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05), 
    .options = furrr_options(seed = TRUE), 
    function(prct) {
  future_map(c("landward", "seaward"), 
      .options = furrr_options(seed = TRUE), 
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

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
