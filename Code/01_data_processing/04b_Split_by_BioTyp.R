#Title -> what is it doing

#Author: Alvise Dabalà
#Date: 21/03/2024

pacman::p_load(tidyverse, sf)

moll_proj <- "ESRI:54009"

PUs <- readRDS("Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds")

PUs_IUCN <- PUs %>%
  st_drop_geometry() %>%
  dplyr::select(contains("Sp_"))

#BioTyp
int_biotyp <- function(BioTyp) {

  sp_BioTyp <- PUs_IUCN %>%
    cbind(PUs %>%
            dplyr::select(all_of(BioTyp)))


  sp_BioTyp %>%
    mutate(across(starts_with("Sp"), ~replace(.,
                                              (. > 0),
                                              .data[[BioTyp]][(. > 0)] %>%
                                                as.numeric))) %>%
    select(starts_with("Sp_")) %>%
    rename_with(~paste0(., "_", BioTyp))

}

sp_biotyp <- map(c("Delta", "Estuary", "Lagoon", "OpenCoast"), int_biotyp) %>%
  bind_cols() %>%
  Filter(function(x) !all(x == 0), .)

PUs <- PUs %>%
  dplyr::select(!(starts_with("Sp_"))) %>%
  dplyr::select(!c("Delta", "Estuary", "Lagoon", "OpenCoast")) %>%
  add_column(sp_biotyp) %>%
  relocate(geometry, .before = ID)

saveRDS(PUs,
        "Results/RDS/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.rds")

st_write(PUs,
         "Results/gpkg/PUs_04a_mangroves_cc_IUCN_split_by_biotyp.gpkg",
         append = FALSE)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
.rs.restartR()
