#Author: Alvise Dabalà
#Date: 15/01/2025

pacman::p_load(sf, tidyverse)

moll_proj <- "ESRI:54009"

dir.create("Data/Demo", recursive = TRUE)

#Country to crop the data
png_map <- rnaturalearth::ne_countries(scale = "large",
                                         country = "papua new guinea",
                                         type = "map_units",
                                         returnclass = "sf") %>%
  st_break_antimeridian(lon_0 = 0) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

crop_function <- function(file_string, name_file, make_valid = FALSE) {
  gmw <- sf::st_read(file_string) %>%
    `if`(make_valid, st_make_valid(.), .) %>%
    st_crop(png_map)

  saveRDS(gmw, paste0("Data/Demo/", name_file, ".rds"))

  rm(gmw)
}

#Global mangrove watch
crop_function("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp", "gmw_png")

#Biophysical typology
crop_function("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp", "biotyp_png",
              make_valid = TRUE)

#IUCN mangrove species
crop_function("Data/IUCN_Distribution_Mangroves/MANGROVES.shp", "IUCN_png",
              make_valid = TRUE)

#WDPA
WDPA_png <- wdpar::wdpa_fetch("papua new guinea")

WDPA_png_1_polygons <- WDPA_png[1:19,]
WDPA_png_2_polygons <- WDPA_png[20:38,]
WDPA_png_3_polygons <- WDPA_png[39:57,]

saveRDS(WDPA_png_1_polygons, "Data/Demo/WDPA_png_1_polygons.rds")
saveRDS(WDPA_png_2_polygons, "Data/Demo/WDPA_png_2_polygons.rds")
saveRDS(WDPA_png_3_polygons, "Data/Demo/WDPA_png_3_polygons.rds")