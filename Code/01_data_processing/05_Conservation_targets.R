#Author: Alvise Dabalà
#Date: 21/02/2024

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Load packages
#pacman::p_load(tidyverse, sf, prioritizr)
library(tidyverse)
library(sf)
library(prioritizr)

cat("\nLoaded prioritizr (and other packages).")

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

PUs_IUCN <- readRDS(file.path(PROCESSED_DATA_DIR, "PUs_02_mangroves_biotyp_cc_IUCN.rds"))

PUs_features <- PUs_IUCN %>%
  select(starts_with("Sp_")) %>%
  st_drop_geometry() %>%
  summarise(across(everything(.), sum)) %>%
  pivot_longer(names_to = "feature", values_to = "AOH", cols = everything(.))

#Calculate species targets following Rodrigues et al. 2014 and Butchart et al. 2015
spp_range_size_km2 <- seq(0.01, max(PUs_features$AOH), by = 100)

spp_target_percentage_rodrigues <-
  loglinear_interpolation(
    x = spp_range_size_km2,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 250000,
    coordinate_two_y = 0.1) * 100

plot(spp_target_percentage_rodrigues ~ spp_range_size_km2)

spp_target_percentage_butchart <- ifelse(
  spp_range_size_km2 >= 10000000,
  (1000000 / spp_range_size_km2) * 100,
  spp_target_percentage_rodrigues)

spp_target_percentage_butchart

# Select the target for each feature
targets_species <- map_dbl(PUs_features$AOH, function(species_AOH) {
  ID_range <- spp_range_size_km2 %>%
    as_tibble() %>%
    mutate(ID = rownames(.)) %>%
    slice(which.min(abs(value - species_AOH))) %>% #Select the value immediately smaller than the exact range of distribution
    dplyr::select(ID) %>%
    as.numeric()

  spp_target_percentage_butchart[[ID_range[[1]]]]/100
})

PUs_features_targets <- PUs_features %>%
  mutate(targets = targets_species)

saveRDS(PUs_features_targets,
        file.path(RESULTS_DIR, "PUs_05_features_targets.rds"))

map(c("PUs_04_mangroves_cc_IUCN_split_by_country_and_biotyp",
      "PUs_04a_mangroves_cc_IUCN_split_by_biotyp"), function(file_name) {

        PUs <- readRDS(paste0(PROCESSED_DATA_DIR, "/", file_name, ".rds"))

        new_file_name <- sub(".*_by", "by", file_name)

        PUs_features_split <- PUs %>%
          select(starts_with("Sp_")) %>%
          st_drop_geometry() %>%
          summarise(across(everything(.), sum)) %>%
          pivot_longer(names_to = "feature",
                       values_to = "AOH",
                       cols = everything(.))

        #Function to assign the targets to subspecies
        split_targets <- function(species) {
          species_target <- PUs_features_targets %>%
            filter(feature == species) %>%
            dplyr::select(targets) %>%
            as.numeric()

          PUs_features_split %>%
            filter(grepl(species, feature)) %>%
            mutate(targets = species_target)
        }

        PUs_features_split_targets <- map(PUs_features$feature, split_targets) %>%
          bind_rows()

        saveRDS(PUs_features_split_targets, paste0(RESULTS_DIR, "/","PUs_05_features_split_targets_",
                                                   new_file_name,
                                                   ".rds"))
      })

cat("\nFinished analysis")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()