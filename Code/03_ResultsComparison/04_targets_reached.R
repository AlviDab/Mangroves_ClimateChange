#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/solution_prioritisation.rds"))

prct <- 0.05

CC_direction <- "mean"

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds"))

solution_cc