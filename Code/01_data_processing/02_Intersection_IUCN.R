#Author: Alvise Dabalà
#Date: 18/04/2023

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Loading packages

#pacman::p_load(tidyverse, sf, tmap)
library(tidyverse)
library(sf)
library(tmap)

# Define projection
moll_proj <- "ESRI:54009"

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


PUs <- read_rds(file.path(PROCESSED_DATA_DIR, "PUs_01_mangroves_biotyp_cc.rds"))

#tictoc::tic()

IUCN_mangroves <- st_read(file.path(RAW_DATA_DIR, "IUCN_Distribution_Mangroves", "MANGROVES.shp")) %>%
  st_transform(moll_proj) %>%
  st_make_valid()

`%!in%` = Negate(`%in%`)

IUCN_mangroves <- IUCN_mangroves %>%
  dplyr::select(sci_name) %>%
  filter(sci_name %!in% c("Acanthus ilicifolius", "Cynometra iripa")) %>%
  mutate(sci_name = str_replace(sci_name, " ", "_")) %>%
  mutate(sci_name = paste0("Sp_", sci_name))

species_names <- IUCN_mangroves$sci_name

PUs_IUCN_index <- PUs %>%
  st_intersects(IUCN_mangroves, sparse = FALSE) %>%
  as_tibble()

#tictoc::toc()

names(PUs_IUCN_index) <- species_names

PUs_IUCN_index <- PUs_IUCN_index %>%
  mutate(n_intersections = rowSums(.)) %>%
  relocate(n_intersections, .before = 'Sp_Rhizophora_apiculata') %>%
  mutate(across(starts_with("Sp_"),
                ~case_when(n_intersections == 0 ~ NA,
                           .default = .)))

PUs_IUCN <- PUs %>%
  cbind(PUs_IUCN_index) %>%
  dplyr::select("ID", "n_intersections",
                starts_with("Sp_"))

#Add using nearest neighborhood for the planning units that don't intersect
source(file.path("f_remove_zeros_nearestneighborhood_IUCN.R"))

PUs_IUCN <- fNN_zeros_IUCN(PUs_IUCN, "n_intersections") %>%
  st_drop_geometry() %>%
  left_join(PUs, by = "ID") %>%
  mutate(across(starts_with("Sp_"),
                ~case_when(.x == "TRUE" ~ as.numeric(area_km2),
                           .default = 0))) %>%
  dplyr::select(!n_intersections) %>%
  st_as_sf()

saveRDS(PUs_IUCN, file.path(RESULTS_DIR, "PUs_02_mangroves_biotyp_cc_IUCN.rds"))

cat("Finished analysis")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()
