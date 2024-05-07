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

PUs <- readRDS("Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds")

PUs_intersecting_WDPA <- PUs %>%
  st_filter(WDPA_PUs, .predicate = st_intersects)

gmw_intersecting_WDPA <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_transform("ESRI:54009") %>%
  st_make_valid() %>%
  st_filter(PUs_intersecting_WDPA, .predicate = st_intersects) %>%
  st_filter(WDPA_PUs, .predicate = st_intersects)

saveRDS(gmw_intersects_WDPA, "Results/RDS/WDPA/gwm_intersects_WDPA.rds")

gmw_intersection_WDPA <- gmw_intersecting_WDPA %>%
  st_intersection(WDPA_PUs)

WDPA_PUs_strict <- WDPA_PUs %>%
  filter(WDPA_PUs$IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))
