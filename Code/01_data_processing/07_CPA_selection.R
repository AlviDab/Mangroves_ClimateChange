#Author: Alvise Dabalà
#Date: 21/02/2024

# Edited by Tin Buenafe 23 May 2025 for HPC functionality

# Load packages
#pacman::p_load(tidyverse, sf, parallel, furrr, purrr)
library(tidyverse)
library(sf)
library(parallelly)
library(furrr)
library(purrr)

# Define directories
args = commandArgs(trailingOnly = TRUE)
RAW_DATA_DIR = args[1] # 1st argument in the srun Rscript function is the the directory where all the raw data are
PROCESSED_DATA_DIR = args[2] # 2nd argument in the srun Rscript function is the directory where all the processed data are
TMP_DIR = Sys.getenv("TMPDIR")
RESULTS_DIR = file.path(TMP_DIR, "Results")

# Create new directories
htr_make_folder <- function(folder) { # Function is from hotrstuff
  if (!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive = TRUE)
}
htr_make_folder(RESULTS_DIR)

#options(future.globals.maxSize = 750*1024^2)

map(c("PUs_04_mangroves_cc_IUCN_split_by_country_and_biotyp",
      "PUs_04a_mangroves_cc_IUCN_split_by_biotyp"
      ), function(file_name) {

        PUs <- readRDS(paste0(PROCESSED_DATA_DIR, "/", file_name, ".rds"))

        split <- sub(".*_by", "by", file_name)
        new_file_name <- sub(".*_m", "m", file_name)

        features <- paste0(PROCESSED_DATA_DIR, "/PUs_05_features_split_targets_",
                           split,
                           ".rds") %>%
          readRDS()

        source("f_find_priority.R")

        #dir.create("Results/RDS/prioritisation_input/Country")
        htr_make_folder(file.path(RESULTS_DIR, "prioritisation_input", "Country"))

        #add mean probability of gain stability
        PUs <- PUs %>%
          mutate(Prob_gain_stability_mean =
                   (Prob_gain_stability_landward + Prob_gain_stability_seaward)/2
          )

        #remove eventual NAs using nearest neighborhood

        #Add using nearest neighborhood the missing values
        source("f_remove_NAs_nearestneighborhood.R")

        PUs <- PUs %>%
          fNN_NAs("Prob_gain_stability_landward") %>%
          fNN_NAs("Prob_gain_stability_seaward") %>%
          fNN_NAs("Prob_gain_stability_mean")

        map(seq(0.05, 1, by = 0.05),
            function(prct) {
              map(c("landward", "seaward",
                    "mean"
                ),
                #.options = furrr_options(seed = TRUE),
                function(CC_direction) {
                  PUs_CC <- f_find_priority(PUs,
                                            paste0("Prob_gain_stability_", CC_direction),
                                            prct,
                                            features)

                  saveRDS(PUs_CC,
                          paste0(RESULTS_DIR, "/prioritisation_input/Country/PUs_05_",
                          new_file_name,
                          "_priority_",
                                 prct, "_", CC_direction, ".rds")
                  )
                })
            })
      })

cat("\nFinished with analysis.")

#plan(sequential)

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()