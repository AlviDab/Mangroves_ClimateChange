#Author: Alvise Dabalà
#Date: 18/04/2023

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Loading packages

# pacman::p_load(sf, terra, tidyverse)
library(sf)
library(terra)
library(tidyverse)

cat("Loaded libraries\n")

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

cat("Defined directories\n")

# Defining projection
moll_proj <- "ESRI:54009"

# Loading files
PUs <- readRDS(file.path(PROCESSED_DATA_DIR, "00_PUs_mollweide.rds"))

predictions_CC <- read.csv(file.path(RAW_DATA_DIR, "Predictions_Buelow", "mangrove-forecasts.csv")) %>%
  as_tibble() %>%
  dplyr::select("ID", "Prob_gain_stability_landward", "Prob_gain_stability_seaward")

# mangroves_biotyp <- readRDS("Results/RDS/mangroves_distribution_mollweide.rds")

# Otherwise just use the shapefile
mangroves_biotyp <- sf::st_read(file.path(RAW_DATA_DIR, "MangroveTypology", "Mangrove_Typology_v3_2020.shp")) %>%
 st_transform(moll_proj) %>%
 st_make_valid()

mangroves_biotyp_cc <- mangroves_biotyp %>%
  left_join(predictions_CC, by = "ID")

PUs <- PUs %>%
  rowid_to_column("ID")

PUs_mangroves_biotyp_cc_intersection <- PUs %>%
  st_intersection(mangroves_biotyp_cc %>%
                    dplyr::select("Class", "Prob_gain_stability_landward",
                                  "Prob_gain_stability_seaward"))

PUs_mangroves_biotyp_cc_intersection <- PUs_mangroves_biotyp_cc_intersection %>%
  mutate(area_km2 = as.numeric(units::set_units(sf::st_area(.), "km2")) %>%
           as.numeric()) %>%
  pivot_wider(names_from = "Class", values_from = "area_km2")

PUs_mangroves_biotyp_cc_intersection <- PUs_mangroves_biotyp_cc_intersection %>%
  rowwise() %>%
  mutate(area_km2 = sum(c_across(Delta:OpenCoast), na.rm = T)) %>%
  group_by(ID) %>%
  summarise(Delta = sum(Delta, na.rm = T),
            Estuary = sum(Estuary , na.rm = T),
            Lagoon = sum(Lagoon , na.rm = T),
            OpenCoast = sum(OpenCoast, na.rm = T),
            Prob_gain_stability_landward = weighted.mean(Prob_gain_stability_landward,
                                                         as.numeric(area_km2),
                                                         na.rm = T), #mean of the probability weighted by the area
            Prob_gain_stability_seaward = weighted.mean(Prob_gain_stability_seaward,
                                                        as.numeric(area_km2),
                                                        na.rm = T),
            area_km2 = sum(area_km2, na.rm = T))

# dir.create("Results/RDS/", recursive = TRUE)
saveRDS(PUs_mangroves_biotyp_cc_intersection, file.path(RESULTS_DIR, "PUs_01_mangroves_biotyp_cc_intersection.rds"))

PUs_mangroves_biotyp_cc <- PUs %>%
  left_join(
    (PUs_mangroves_biotyp_cc_intersection %>%
       st_drop_geometry() %>%
       as_tibble()
    ), by = "ID")

saveRDS(PUs_mangroves_biotyp_cc, file.path(RESULTS_DIR, "PUs_01_mangroves_biotyp_cc.rds"))

st_write(PUs_mangroves_biotyp_cc,
        file.path(RESULTS_DIR, "PUs_01_mangroves_biotyp_cc.gpkg"),
        append = FALSE)

cat("Finished analysis")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()
