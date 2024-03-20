#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05),
           function(prct) {
             CC_direction <- "mean"

             solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/solution_prioritisation.rds"))

             solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                           CC_direction, "/solution_",
                                           as.character(prct), "_", CC_direction, ".rds"))

             #area climate-naïve
             area_cn <- solution %>%
               as_tibble() %>%
               group_by(solution_1) %>%
               summarise(area_km2 = sum(area_km2)) %>%
               mutate(prct_area = area_km2/sum(area_km2)*100)

             #area climate-smart
             area_cs <- solution_cc %>%
               as_tibble() %>%
               group_by(solution_1) %>%
               summarise(area_km2 = sum(area_km2)) %>%
               mutate(prct_area = area_km2/sum(area_km2)*100)

             #percentage difference
             diff <- area_cs$area_km2[2] - area_cn$area_km2[2]
             diff/area_cn$area_km2[2]*100

             barplot_area <- area_cn
           })