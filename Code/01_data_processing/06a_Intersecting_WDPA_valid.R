#Author: Alvise Dabalà
#Date: 26/04/2024

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Load packages
#pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)
library(tidyverse)
library(sf)
library(parallelly)
library(furrr)
library(purrr)
library(wdpar)

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

PUs <- readRDS(file.path(PROCESSED_DATA_DIR, "PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds")) %>%
  st_transform("ESRI:54017") %>%
  mutate(valid_geom = st_is_valid(.))

PUs_valid <- PUs %>%
  filter(valid_geom == TRUE)

'%!in%' <- function(x,y)!('%in%'(x,y))

PUs_not_valid <- PUs %>%
  filter(ID %!in% PUs_valid$ID)

#ncores <- parallelly::availableCores(method = "Slurm", omit = 2) # using parallelly instead of parallel to account for different cores in the HPC
#ncores <- detectCores() - 2

#plan(multisession, workers = ncores)

## polygons

#I KEEP IT NOT PARALLEL CAUSE I CAN'T OPEN THE THREE FILES AT THE SAME TIME
#can parallelise using future_map and removing part of the code with '#'

cat("\nStart parallel functions")

map(c("polygons",
  "points"), function(shape) {
  map(0:2,
      #.options = furrr_options(seed = TRUE),
      function(number_file) {

        WDPA <- st_read(paste0(RAW_DATA_DIR, "/WDPA/WDPA_WDOECM_Apr2024_Public_all_shp/WDPA_WDOECM_Apr2024_Public_all_shp_",
                               number_file,
                               "/WDPA_WDOECM_Apr2024_Public_all_shp-", shape, ".shp")) %>%
          wdpar::wdpa_clean(erase_overlaps = FALSE)

        cat("\nloaded WDPA")

        #dir.create("Results/RDS/WDPA/cleaned_map_ESRI_54017/", recursive = TRUE)

        saveRDS(WDPA,
                paste0(RESULTS_DIR, "/WDPA_", shape,"_clean_", number_file, ".rds"))

        cat("\nsaved WDPA")

        WDPA_PUs_int_filter <- WDPA %>%
          st_filter(PUs_valid, .predicate = st_intersects)

        #dir.create("Results/RDS/WDPA/PUs_valid/filtered/", recursive = TRUE)

        htr_make_folder(file.path(RESULTS_DIR, "PUs_valid"))

        saveRDS(WDPA_PUs_int_filter,
                paste0(RESULTS_DIR, "/PUs_valid/WDPA_", shape,"_filtered_", number_file, ".rds"))

        cat("\nsaved WDPA filtered")
      })
})

#plan(sequential)

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()
