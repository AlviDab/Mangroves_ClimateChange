#Author: Alvise Dabalà
#Date: 18/04/2023

pacman::p_load(tidyverse, sf, tmap)

moll_proj <- "ESRI:54009"

PUs <- read_rds("Results/RDS/PUs_01_mangroves_biotyp_cc.rds")

tictoc::tic()

IUCN_mangroves <- st_read("Data/IUCN_Distribution_Mangroves/MANGROVES.shp") %>% 
  st_transform(moll_proj) %>% 
  st_make_valid()

tictoc::toc()

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

#Add using nearest neighborhood the missing values
source("Code/Functions/fRemove_NANearestNeighbourg_IUCN.R")

PUs_IUCN <- fNN_IUCN(PUs_IUCN, "n_intersections") %>% 
  st_drop_geometry() %>% 
  left_join(PUs, by = "ID") %>% 
  mutate(across(starts_with("Sp_"), 
                ~case_when(.x == "TRUE" ~ as.numeric(area_km2),
                           .default = 0))) %>% 
  dplyr::select(!n_intersections) %>% 
  st_as_sf()

saveRDS(PUs_IUCN, "Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
