#Author: Alvise Dabalà
#Date: 29/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)

PUs <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds") %>%
  st_transform("ESRI:54017") %>%
  mutate(valid_geom = st_is_valid(.))

PUs_valid <- PUs %>%
  filter(valid_geom == TRUE)

'%!in%' <- function(x,y)!('%in%'(x,y))

PUs_not_valid <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds") %>%
  filter(ID %!in% PUs_valid$ID)

dir.create("Results/RDS/WDPA/PUs_not_valid/", recursive = TRUE)

#Check the region
st_write(PUs_not_valid %>%
           dplyr::select(ID), "Results/RDS/WDPA/PUs_not_valid/PUs_not_valid.gpkg",
         append = TRUE) #The region is Fiji

map(c("polygons", "points"), function(shape) {
  map(0:2,
      #.options = furrr_options(seed = TRUE),
      function(number_file) {

        WDPA <- st_read(paste0("Data/WDPA/WDPA_WDOECM_Apr2024_Public_FJI_shp/WDPA_WDOECM_Apr2024_Public_FJI_shp_",
                                    number_file,
                                    "/WDPA_WDOECM_Apr2024_Public_FJI_shp-", shape,".shp")) %>%
          wdpar::wdpa_clean(erase_overlaps = FALSE, crs = "ESRI:54009")

        WDPA_PUs_int_filter <- WDPA %>%
          st_filter(PUs_not_valid, .predicate = st_intersects)

        dir.create("Results/RDS/WDPA/PUs_not_valid/filtered/", recursive = TRUE)

        saveRDS(WDPA_PUs_int_filter,
                paste0("Results/RDS/WDPA/PUs_not_valid/filtered/WDPA_FJI_", shape,"_filtered_", number_file, ".rds"))
      })
})

# plan(sequential)
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()