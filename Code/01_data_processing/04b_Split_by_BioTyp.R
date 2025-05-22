#Author: Alvise Dabalà
#Date: 21/03/2024

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Load packages
#pacman::p_load(tidyverse, sf)
library(tidyverse)
library(sf)

# Define projections
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

PUs <- readRDS(file.path(PROCESSED_DATA_DIR, "PUs_02_mangroves_biotyp_cc_IUCN.rds"))

PUs_IUCN <- PUs %>%
  st_drop_geometry() %>%
  dplyr::select(contains("Sp_"))

#BioTyp
int_biotyp <- function(BioTyp) {

  sp_BioTyp <- PUs_IUCN %>%
    cbind(PUs %>%
            dplyr::select(all_of(BioTyp)))


  sp_BioTyp %>%
    mutate(across(starts_with("Sp"), ~replace(.,
                                              (. > 0),
                                              .data[[BioTyp]][(. > 0)] %>%
                                                as.numeric))) %>%
    select(starts_with("Sp_")) %>%
    rename_with(~paste0(., "_", BioTyp))

}

sp_biotyp <- map(c("Delta", "Estuary", "Lagoon", "OpenCoast"), int_biotyp) %>%
  bind_cols() %>%
  Filter(function(x) !all(x == 0), .)

PUs <- PUs %>%
  dplyr::select(!(starts_with("Sp_"))) %>%
  dplyr::select(!c("Delta", "Estuary", "Lagoon", "OpenCoast")) %>%
  add_column(sp_biotyp) %>%
  relocate(geometry, .before = ID)

saveRDS(PUs,
        file.path(RESULTS_DIR, "PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds"))

st_write(PUs,
        file.path(RESULTS_DIR, "PUs_04a_mangroves_cc_IUCN_split_by_biotyp.gpkg"),
         append = FALSE)

cat("Finished analysis")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memory and report the memory usage.
#.rs.restartR()
