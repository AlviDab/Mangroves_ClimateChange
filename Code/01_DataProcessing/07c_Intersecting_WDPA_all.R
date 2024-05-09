#Author: Alvise Dabalà
#Date: 29/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)

WDPA_PUs_valid_names <- list.files("Results/RDS/WDPA/PUs_valid/filtered",
                        pattern = "*.rds", full.names = TRUE)

WDPA_PUs_not_valid_names <- list.files("Results/RDS/WDPA/PUs_not_valid/filtered",
                                 pattern = "*.rds", full.names = TRUE)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

WDPA_PUs <- c(WDPA_PUs_valid_names, WDPA_PUs_not_valid_names) %>%
  future_map(.options = furrr_options(seed = TRUE), function(name_file) {

    WDPA_PUs <- readRDS(name_file)

    if(grepl("PUs_valid", name_file) == TRUE) {
      WDPA_PUs <- WDPA_PUs %>%
        st_transform(crs = 4326) %>%
        wdpa_clean(erase_overlaps = FALSE, crs = "ESRI:54009")
    }

    return(WDPA_PUs)
  })

plan(sequential)

WDPA_PUs <- WDPA_PUs[unlist(map(WDPA_PUs, ~(nrow(.)) > 0))] %>%
  bind_rows()

saveRDS(WDPA_PUs, "Results/RDS/WDPA/all_overlapping_MPAs_ESRI_54009.rds")

WDPA_PUs <- readRDS("Results/RDS/WDPA/all_overlapping_MPAs_ESRI_54009.rds") #TO REMOVE

PUs <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds")

# WDPA_PUs_union <- WDPA_PUs %>%
#   wdpar::wdpa_dissolve()
#
# saveRDS(WDPA_PUs_union, "Results/RDS/WDPA/all_overlapping_MPAs_ESRI_54009_union.rds")
#
# rm(WDPA_PUs)

gmw_intersection_WDPA <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_transform("ESRI:54009") %>%
  st_make_valid() %>%
  st_intersection(WDPA_PUs)

saveRDS(gmw_intersection_WDPA, "Results/RDS/WDPA/gmw_intersection_WDPA.rds")

# WDPA_PUs_strict <- WDPA_PUs %>%
#   filter(WDPA_PUs$IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))
#
# PUs_intersects_WDPA <- PUs %>%
#   st_filter(WDPA_PUs_union, .predicate = st_intersects)
#
# saveRDS(PUs_intersects_WDPA, "Results/RDS/WDPA/PUs_intersects_WDPA.rds")
#
# gmw_intersects_WDPA <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
#   st_transform("ESRI:54009") %>%
#   st_make_valid() %>%
#   st_filter(PUs_intersects_WDPA, .predicate = st_intersects)
#
# saveRDS(gmw_intersects_protectedPUs, "Results/RDS/WDPA/gwm_intersects_protectedPUs.rds")
#
# gmw_intersects_WDPA <- gmw_intersects_protectedPUs %>%
#   st_filter(WDPA_PUs, .predicate = st_intersects)
#
# saveRDS(gmw_intersects_WDPA, "Results/RDS/WDPA/gwm_intersects_WDPA.rds")
