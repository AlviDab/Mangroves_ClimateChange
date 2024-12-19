#Author: Alvise Dabalà
#Date: 19/02/2024

pacman::p_load(tidyverse, sf)

moll_proj <- "ESRI:54009"

PUs <- readRDS("Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds")

MEOW <- read_sf("Data/MEOW/Marine_Ecoregions_Of_the_World__MEOW_.shp") %>%
  st_transform(moll_proj) %>%
  st_make_valid()

PUs_MEOW_index <- PUs %>%
  st_centroid() %>% #so that it intersect only one ecoregion
  st_intersects(MEOW) %>%
  as_vector()

MEOW_ecoregions <- MEOW %>%
  st_drop_geometry() %>%
  dplyr::select(ECOREGION) %>%
  mutate(ECOREGION = paste0("MEOW_",
                            str_replace_all(MEOW$ECOREGION, " ", "_") %>% #To not have any column with ``
                              str_replace_all("/", "_and_")))

PUs_MEOW <- PUs %>%
  mutate(MEOW_ecoregions[PUs_MEOW_index, ]) %>%
  rename(MEOW = ECOREGION) %>%
  relocate(geometry, .before = "ID") %>%
  relocate(MEOW, .before = "area_km2") %>%
  st_as_sf()

saveRDS(PUs_MEOW, "Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()