#Author: Alvise Dabalà
#Date: 29/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, wdpar)

WDPA_PUs_valid_names <- list.files("Results/RDS/WDPA/PUs_valid/filtered",
                                   pattern = "*.rds", full.names = TRUE)

WDPA_PUs_not_valid_names <- list.files("Results/RDS/WDPA/PUs_not_valid/filtered",
                                       pattern = "*.rds", full.names = TRUE)

#more explanation here. This is depending on the local machine. Many people don't even have enough cores to do -10
ncores <- detectCores() - 10 # I'd probably say -2 here to be on the save side for all machines

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

biotyp_intersection_WDPA <- st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>%
  st_transform("ESRI:54009") %>%
  st_make_valid() %>%
  st_intersection(WDPA_PUs)

saveRDS(biotyp_intersection_WDPA, "Results/RDS/WDPA/biotyp_intersection_WDPA.rds")

biotyp_intersection_WDPA <- readRDS("Results/RDS/WDPA/biotyp_intersection_WDPA.rds")

#Divide by Class
biotyp_intersection_WDPA <- biotyp_intersection_WDPA %>%
  group_split(Class)

group_name <- map(biotyp_intersection_WDPA, function(x) {
  x$Class %>%
    unique()
}
) %>% unlist()

plan(multisession, workers = ncores)

#Union by class to avoid overlap of PAs
biotyp_intersection_WDPA_all_union <- biotyp_intersection_WDPA %>%
  future_map(sf::st_union)

plan(sequential)

saveRDS(biotyp_intersection_WDPA_all_union, "Results/RDS/WDPA/biotyp_intersection_WDPA_all_union.rds")

biotyp_intersection_WDPA_all_union <- readRDS("Results/RDS/WDPA/biotyp_intersection_WDPA_all_union.rds")

plan(multisession, workers = ncores)

PUs_biotyp_WDPA_all_intersection <- biotyp_intersection_WDPA_all_union %>%
  seq_along() %>%
  future_map(function(class_index) {

    biotyp_intersection_WDPA_all_group <- biotyp_intersection_WDPA_all_union[[class_index]] %>%
      st_as_sf() %>%
      st_cast("POLYGON")

    class_name <- group_name[[class_index]]

    biotyp_intersection_WDPA_all_group <- biotyp_intersection_WDPA_all_group %>%
      st_intersection(PUs) %>%
      mutate(!!sym(paste0(class_name, "_WDPA_km2")) := st_area(.) %>%
               units::set_units(km^2) %>%
               as.numeric(.)) %>%
      st_drop_geometry() %>%
      group_by(ID) %>%
      summarise(!!sym(paste0(class_name, "_WDPA_all_km2")) :=
                  sum(!!sym(paste0(class_name, "_WDPA_km2"))))

  })

plan(sequential)

saveRDS(PUs_biotyp_WDPA_all_intersection, "Results/RDS/WDPA/PUs_biotyp_WDPA_all_intersection.rds")

PUs_biotyp_WDPA_all_intersection <- readRDS("Results/RDS/WDPA/PUs_biotyp_WDPA_all_intersection.rds")

rm(biotyp_intersection_WDPA_all_union)

# #For I-VI
# gmw_intersection_WDPA_I_VI_union <- gmw_intersection_WDPA %>%
#   filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI")) %>%
#   st_union()
#
# saveRDS(gmw_intersection_WDPA_I_VI_union, "Results/RDS/WDPA/gmw_intersection_WDPA_I_VI_union.rds")
#
# PUs_gmw_WDPA_I_VI_intersection <- PUs %>%
#   st_intersection(gmw_intersection_WDPA_I_VI_union %>%
#                     st_cast("POLYGON")) %>%
#   mutate(area_mangroves_WDPA_km2 = st_area(.) %>%
#            units::set_units(km^2)) %>%
#   st_drop_geometry() %>%
#   group_by(ID) %>%
#   summarise(area_mangroves_WDPA_I_VI_km2 = sum(area_mangroves_WDPA_km2))
#
# saveRDS(PUs_gmw_WDPA_I_VI_intersection, "Results/RDS/WDPA/PUs_gmw_WDPA_I_VI_intersection.rds")
#
# PUs_gmw_WDPA_I_VI_intersection <- readRDS("Results/RDS/WDPA/PUs_gmw_WDPA_I_VI_intersection.rds")
#
# rm(gmw_intersection_WDPA_I_VI_union)
#
# #Also for I-IV (strictly protected)
# gmw_intersection_WDPA_I_IV_union <- gmw_intersection_WDPA %>%
#   filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>%
#   st_union()
#
# saveRDS(gmw_intersection_WDPA_I_IV_union, "Results/RDS/WDPA/gmw_intersection_WDPA_I_IV_union.rds")
#
# PUs_gmw_WDPA_I_IV_intersection <- PUs %>%
#   st_intersection(gmw_intersection_WDPA_I_IV_union %>%
#                     st_cast("POLYGON")) %>%
#   mutate(area_mangroves_WDPA_km2 = st_area(.) %>%
#            units::set_units(km^2)) %>%
#   st_drop_geometry() %>%
#   group_by(ID) %>%
#   summarise(area_mangroves_WDPA_I_IV_km2 = sum(area_mangroves_WDPA_km2))
#
# saveRDS(PUs_gmw_WDPA_I_IV_intersection, "Results/RDS/WDPA/PUs_gmw_WDPA_I_IV_intersection.rds")
#
# rm(gmw_intersection_WDPA_I_IV_union)

#Add to the planning units
biotyp_WDPA_km2 <- PUs_biotyp_WDPA_all_intersection[[1]]

# some more explanation needed here
for(i in 2:4) {
  biotyp_WDPA_km2 <- biotyp_WDPA_km2 %>%
    full_join(PUs_biotyp_WDPA_all_intersection[[i]],
              by = "ID")
}

biotyp_WDPA_km2 <- biotyp_WDPA_km2 %>%
  mutate(across(.cols = -c(ID),
                .fns = as.numeric)) %>%
  mutate(across(.cols = -c(ID),
                .fns = ~ ifelse(is.na(.), 0, .)))

PUs <- PUs %>%
  left_join(biotyp_WDPA_km2, by = "ID") %>%
  mutate(across(ends_with("WDPA_all_km2"),
                .fns = ~ ifelse(is.na(.), 0, .)))

#Check total mangrove area covered
PUs %>%
  st_drop_geometry() %>%
  select(ends_with("WDPA_all_km2")) %>%
  rowwise() %>%
  summarise(WDPA_area = rowSums(pick(where(is.numeric)))) %>%
  sum()

saveRDS(PUs, "Results/RDS/PUs_06_cc_IUCN_split_by_biotyp_WDPA.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()