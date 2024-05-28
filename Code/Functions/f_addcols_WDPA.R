#Author: Alvise Dabalà
#Date: 28/05/2024

f_addcols_WDPA <- function(sol) {

  PUs_WDPA <- readRDS("Results/RDS/PUs_06_cc_IUCN_split_by_MEOW_and_biotyp_WDPA.rds") %>%
    dplyr::select(c(area_mangroves_WDPA_all_km2,
                    area_mangroves_WDPA_I_VI_km2,
                    area_mangroves_WDPA_I_IV_km2,
                    ID)) %>%
    st_drop_geometry()

  #add column WDPA by ID
  sol <- sol %>%
    left_join(PUs_WDPA, by = "ID") %>%
    mutate(WDPA_all = case_when(area_mangroves_WDPA_all_km2 >= MangroveArea_km2/2 ~ TRUE,
                                .default = FALSE),
           WDPA_I_VI = case_when(area_mangroves_WDPA_I_VI_km2 >= MangroveArea_km2/2 ~ TRUE,
                                .default = FALSE),
           WDPA_I_IV = case_when(area_mangroves_WDPA_I_IV_km2 >= MangroveArea_km2/2 ~ TRUE,
                                .default = FALSE))

  return(sol)
}