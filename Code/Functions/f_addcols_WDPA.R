#Author: Alvise Dabalà
#Date: 28/05/2024
#Description: Function to add WDPA columns

################################################################################

f_addcols_WDPA <- function(sol) {

  PUs_WDPA <- readRDS("Results/RDS/PUs_06_cc_IUCN_split_by_biotyp_WDPA.rds") %>%
    dplyr::select(ends_with("WDPA_all_km2"),
                  ID) %>%
    st_drop_geometry()

  sol <- sol %>%
    left_join(PUs_WDPA, by = "ID")

  return(sol)
}
