#Author: Alvise Dabalà
#Date: 20/02/2024

# Edited by Tin Buenafe 22 May 2025 for HPC functionality

# Load packages
#pacman::p_load(tidyverse, sf, purrr)
library(tidyverse)
library(sf)
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

PUs <- readRDS(file.path(PROCESSED_DATA_DIR, "PUs_02_mangroves_biotyp_cc_IUCN.rds"))

source("f_intersect_countries.R")

#Intersect countries
PUs <- PUs %>%
  f_int_countries()

#Species
sp_names <- PUs %>%
  st_drop_geometry() %>%
  dplyr::select(starts_with("Sp")) %>%
  names()

split_species <- function(x) {
  sp_country <- PUs %>%
    dplyr::select(all_of(x), country)

  sp_country %>%
    pivot_wider(names_from = "country",
                values_from = x,
                names_glue = paste0(x, "_{country}")
    ) %>%
    st_drop_geometry()
}

sp_country <- map(sp_names, ~split_species(.x)) %>%
  bind_cols() %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)) #replace all the NA with 0
  ) %>%
  Filter(function(x) !all(x == 0), .) #remove col of all zeros

#BioTyp
int_biotyp <- function(BioTyp) {

  sp_BioTyp <- sp_country %>%
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

sp_country_biotyp <- map(c("Delta", "Estuary", "Lagoon", "OpenCoast"), int_biotyp) %>%
  bind_cols() %>%
  Filter(function(x) !all(x == 0), .)

PUs <- PUs %>%
  dplyr::select(!(starts_with("Sp_"))) %>%
  dplyr::select(!c("country", "Delta", "Estuary", "Lagoon", "OpenCoast")) %>%
  add_column(sp_country_biotyp) %>%
  relocate(geometry, .before = ID)

saveRDS(PUs,
        file.path(RESULTS_DIR, "PUs_04_mangroves_cc_IUCN_split_by_country_and_biotyp.rds"))

cat("Finished analysis")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.
#.rs.restartR()
