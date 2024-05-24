#Author: Alvise Dabalà
#Date: 14/05/2024

pacman::p_load(tidyverse, sf, furrr, purrr, parallel)

split_group <- "biotyp"

dir.create(paste0("Figures/Country/10_plot_comparison_area_lw_sw/",
                  split_group), recursive = TRUE)

kappa_cc <- map(seq(0.05, 0.3, by = 0.05),
                function(prct) {

                  solution_cc_sw <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                   split_group, "/seaward/solution_",
                                                   as.character(prct), "_seaward.rds")) %>%
                    mutate(type = "seaward")

                  solution_cc_lw <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                   split_group, "/landward/solution_",
                                                   as.character(prct), "_landward.rds")) %>%
                    mutate(type = "landward")

                  kappa <- spatialplanr::splnr_get_kappaCorrData(list(solution_cc_lw,
                                                                      solution_cc_sw),
                                                                 c("landward", "seaward"))[[2]]

                  write(paste0("Cohen's Kappa ", as.character(prct), " thresholds = ", as.character(kappa)), append = TRUE,
                        file = paste0("Figures/Country/10_plot_comparison_area_lw_sw/",
                                      split_group, "/Cohens_kappa.txt"))

                  return(kappa)
                }) %>%
  unlist()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()