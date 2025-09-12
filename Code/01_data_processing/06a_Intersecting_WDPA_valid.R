#Author: Alvise Dabalà
#Date: 26/04/2024
#Description: This script intersects the valid WDPA polygons with the valid PUs
#             to extract only the protected areas that overlap with the PUs

################################################################################

# Load packages
pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)

# Load PUs and transform to the crs of the WDPA
PUs <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds") %>%
  st_transform("ESRI:54017") %>%
  mutate(valid_geom = st_is_valid(.))

# Select only valid geometries
PUs_valid <- PUs %>%
  filter(valid_geom == TRUE)

# Select the PUs that are not valid (to check them later in 06b)
'%!in%' <- function(x,y)!('%in%'(x,y))

PUs_not_valid <- PUs %>%
  filter(ID %!in% PUs_valid$ID)

# ncores <- detectCores() - 2

# plan(multisession, workers = ncores)

# I KEEP IT NOT PARALLEL CAUSE I CAN'T OPEN THE THREE FILES AT THE SAME TIME
# can parallelise using future_map and removing part of the code with '#'

# Intersect WDPA with PUs_valid
map(c("polygons",
  "points"), function(shape) {
  map(0:2,
      #.options = furrr_options(seed = TRUE),
      function(number_file) {

        WDPA <- st_read(paste0("Data/WDPA/WDPA_WDOECM_Apr2024_Public_all_shp/WDPA_WDOECM_Apr2024_Public_all_shp_",
                               number_file,
                               "/WDPA_WDOECM_Apr2024_Public_all_shp-", shape, ".shp")) %>%
          wdpar::wdpa_clean(erase_overlaps = FALSE)

        dir.create("Results/RDS/WDPA/cleaned_map_ESRI_54017/", recursive = TRUE)

        saveRDS(WDPA,
                paste0("Results/RDS/WDPA/cleaned_map_ESRI_54017/WDPA_", shape,"_clean_", number_file, ".rds"))

        WDPA_PUs_int_filter <- WDPA %>%
          st_filter(PUs_valid, .predicate = st_intersects)

        dir.create("Results/RDS/WDPA/PUs_valid/filtered/", recursive = TRUE)

        saveRDS(WDPA_PUs_int_filter,
                paste0("Results/RDS/WDPA/PUs_valid/filtered/WDPA_", shape,"_filtered_", number_file, ".rds"))
      })
})

# plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
