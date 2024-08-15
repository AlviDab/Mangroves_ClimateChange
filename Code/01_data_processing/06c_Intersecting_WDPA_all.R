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

gmw_intersection_WDPA <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_transform("ESRI:54009") %>%
  st_make_valid() %>%
  st_intersection(WDPA_PUs)

saveRDS(gmw_intersection_WDPA, "Results/RDS/WDPA/gmw_intersection_WDPA.rds")

gmw_intersection_WDPA <- readRDS("Results/RDS/WDPA/gmw_intersection_WDPA.rds")

#For all
gmw_intersection_WDPA_all_union <- gmw_intersection_WDPA %>%
  st_union()

saveRDS(gmw_intersection_WDPA_all_union, "Results/RDS/WDPA/gmw_intersection_WDPA_all_union.rds")

PUs_gmw_WDPA_all_intersection <- PUs %>%
  st_intersection(gmw_intersection_WDPA_all_union %>%
                    st_cast("POLYGON")) %>%
  mutate(area_mangroves_WDPA_km2 = st_area(.) %>%
           units::set_units(km^2)) %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(area_mangroves_WDPA_all_km2 = sum(area_mangroves_WDPA_km2))

saveRDS(PUs_gmw_WDPA_all_intersection, "Results/RDS/WDPA/PUs_gmw_WDPA_all_intersection.rds")

PUs_gmw_WDPA_all_intersection <- readRDS("Results/RDS/WDPA/PUs_gmw_WDPA_all_intersection.rds")

rm(gmw_intersection_WDPA_all_union)

#For I-VI
gmw_intersection_WDPA_I_VI_union <- gmw_intersection_WDPA %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI")) %>%
  st_union()

saveRDS(gmw_intersection_WDPA_I_VI_union, "Results/RDS/WDPA/gmw_intersection_WDPA_I_VI_union.rds")

PUs_gmw_WDPA_I_VI_intersection <- PUs %>%
  st_intersection(gmw_intersection_WDPA_I_VI_union %>%
                    st_cast("POLYGON")) %>%
  mutate(area_mangroves_WDPA_km2 = st_area(.) %>%
           units::set_units(km^2)) %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(area_mangroves_WDPA_I_VI_km2 = sum(area_mangroves_WDPA_km2))

saveRDS(PUs_gmw_WDPA_I_VI_intersection, "Results/RDS/WDPA/PUs_gmw_WDPA_I_VI_intersection.rds")

PUs_gmw_WDPA_I_VI_intersection <- readRDS("Results/RDS/WDPA/PUs_gmw_WDPA_I_VI_intersection.rds")

rm(gmw_intersection_WDPA_I_VI_union)

#Also for I-IV (strictly protected)
gmw_intersection_WDPA_I_IV_union <- gmw_intersection_WDPA %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>%
  st_union()

saveRDS(gmw_intersection_WDPA_I_IV_union, "Results/RDS/WDPA/gmw_intersection_WDPA_I_IV_union.rds")

PUs_gmw_WDPA_I_IV_intersection <- PUs %>%
  st_intersection(gmw_intersection_WDPA_I_IV_union %>%
                    st_cast("POLYGON")) %>%
  mutate(area_mangroves_WDPA_km2 = st_area(.) %>%
           units::set_units(km^2)) %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(area_mangroves_WDPA_I_IV_km2 = sum(area_mangroves_WDPA_km2))

saveRDS(PUs_gmw_WDPA_I_IV_intersection, "Results/RDS/WDPA/PUs_gmw_WDPA_I_IV_intersection.rds")

rm(gmw_intersection_WDPA_I_IV_union)

#Add to the planning units
PUs <- PUs %>%
  left_join(PUs_gmw_WDPA_all_intersection, by = "ID") %>%
  left_join(PUs_gmw_WDPA_I_IV_intersection, by = "ID") %>%
  left_join(PUs_gmw_WDPA_I_VI_intersection, by = "ID") %>%
  mutate(across(c(area_mangroves_WDPA_all_km2,
                  area_mangroves_WDPA_I_VI_km2,
                  area_mangroves_WDPA_I_IV_km2), ~ ifelse(is.na(.), 0, .)))

saveRDS(PUs, "Results/RDS/PUs_06_cc_IUCN_split_by_MEOW_and_biotyp_WDPA.rds")
