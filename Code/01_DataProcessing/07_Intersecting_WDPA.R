#Author: Alvise Dabalà
#Date: 26/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)

PUs <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds")

#ncores <- detectCores() - 2

#plan(multisession, workers = ncores)

## polygons

#I KEEP IT NOT PARALLEL CAUSE I CAN'T OPEN THE THREE FILES AT THE SAME TIME
#can parallelise using future_map and removing part of the code with '#'
map(0:2,
    #.options = furrr_options(seed = TRUE),
    function(number_file) {

WDPA <- st_read(paste0("Data/WDPA/WDPA_WDOECM_Apr2024_Public_all_shp/WDPA_WDOECM_Apr2024_Public_all_shp_",
                       number_file,
                       "/WDPA_WDOECM_Apr2024_Public_all_shp-polygons.shp")) %>%
  wdpar::wdpa_clean(crs = "ESRI:54009", erase_overlaps = FALSE)

dir.create("Results/RDS/WDPA/cleaned_map/", recursive = TRUE)

saveRDS(WDPA,
        paste0("Results/RDS/WDPA/cleaned_map/WDPA_clean_", number_file, ".rds"))

WDPA_PUs_int_filter <- WDPA %>%
  st_filter(PUs, .predicate = st_intersects)

dir.create("Results/RDS/WDPA/PUs/filtered/", recursive = TRUE)

saveRDS(WDPA_PUs_int_filter,
        paste0("Results/RDS/WDPA/PUs/filtered/WDPA_polygons_filtered_", number_file, ".rds"))
})

# plan(sequential)