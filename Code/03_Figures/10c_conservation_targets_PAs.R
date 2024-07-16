#Author: Alvise Dabalà
#Date: 16/07/2024

pacman::p_load(tidyverse, sf)

source("Code/Functions/f_addcols_WDPA.r")

PUs_WDPA <- readRDS("Results/RDS/PUs_06_cc_IUCN_split_by_MEOW_and_biotyp_WDPA.rds") %>%
  st_drop_geometry()

names_sp <- PUs_WDPA %>%
  dplyr::select(starts_with("Sp")) %>%
  names()

prct_protection_sp <- map(names_sp, function(sp) {

  PUs_WDPA_prct_protection_sp <- PUs_WDPA %>%
    dplyr::select(all_of(sp), area_mangroves_WDPA_I_VI_km2) %>%
    mutate(area_mangroves_WDPA_I_VI_km2 = case_when(
      !!sym(sp) > 0 ~ area_mangroves_WDPA_I_VI_km2,
      .default = 0)) %>%
    summarise(!!sym(sp) := sum(area_mangroves_WDPA_I_VI_km2)/sum(!!sym(sp)))

}) %>%
  bind_cols() %>%
  pivot_longer(cols = everything(.), names_to = "feature", values_to = "prct_protection")

#!!! NEED TO SOLVE PROBLEM OVERLAP between the protected area and the subspecies
#(the code does not consider the percentage of overlap with the biophysical typology)

PUs_features_split_targets <- readRDS("Results/RDS/PUs_05_features_split_targets_by_biotyp.rds")

prct_protection_sp <- prct_protection_sp %>%
  left_join(PUs_features_split_targets, by = "feature") %>%
  mutate(protected = case_when(
    prct_protection >= targets ~ TRUE,
    .default = FALSE
  ))

sum(prct_protection_sp$protected)/258