# Author: Alvise Dabalà
# Date: 14/05/2024
# Description: This script calculates the overall Cohen's Kappa between landward and seaward prioritisation solutions at different climate-smart thresholds.

###################################################################################

# Load packages
pacman::p_load(tidyverse, sf, furrr, purrr, parallel, patchwork)

# If not installed, install irr package
if (!require(irr)) {
  install.packages("irr")
}

# Set parameters
split_group <- "biotyp"
targets <- "targets_area"

# Directory to save results
dir.create(paste0(
  "Figures/Country/04_plot_comparison_area_lw_sw/",
  split_group, "_", targets
), recursive = TRUE)

# Make calculations of Cohen's Kappa
kappa_cc <- map(
  seq(0.05, 1, by = 0.05),
  function(prct) {
    solution_cc_sw <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group, "_", targets, "/seaward/solution_",
      as.character(prct), "_seaward.rds"
    )) %>%
      mutate(type = "seaward")

    solution_cc_lw <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group, "_", targets, "/landward/solution_",
      as.character(prct), "_landward.rds"
    )) %>%
      mutate(type = "landward")

    kappa <- spatialplanr::splnr_get_kappaCorrData(
      list(
        solution_cc_lw,
        solution_cc_sw
      ),
      c("landward", "seaward")
    )[[2]]

    write(paste0("Cohen's Kappa ", as.character(prct), " thresholds = ", as.character(kappa)),
      append = TRUE,
      file = paste0(
        "Figures/Country/04_plot_comparison_area_lw_sw/",
        split_group, "_", targets, "/Cohens_kappa.txt"
      )
    )

    return(kappa)
  }
) %>%
  unlist()

# Clean up R environment
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() # free up memrory and report the memory usage.
.rs.restartR()
