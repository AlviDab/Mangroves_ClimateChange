#Author: Alvise Dabalà
#Date: 20/02/2024

pacman::p_load(tidyverse, sf, purrr)

PUs <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds")

source("Code/Functions/f_intersect_countries.R")

#Intersect countries
PUs <- PUs %>%
  f_int_countries() %>%
  mutate(country = case_when(country == "France" ~ "French Guiana",
                             .default = country) %>%
           gsub(" ", "_", .))

#Species
sp_names <- PUs %>%
  st_drop_geometry() %>%
  dplyr::select(starts_with("Sp")) %>%
  names()

split_species <- function(x) {
  sp_country <- PUs %>%
    dplyr::select(all_of(x), country)

  sp_country %>%
    pivot_wider(names_from = "country",
                values_from = x,
                names_glue = paste0(x, "_{country}")
    ) %>%
    st_drop_geometry()
}

sp_country <- map(sp_names, ~split_species(.x)) %>%
  bind_cols() %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)) #replace all the NA with 0
  ) %>%
  Filter(function(x) !all(x == 0), .) #remove col of all zeros

#BioTyp
int_biotyp <- function(BioTyp) {

  sp_BioTyp <- sp_country %>%
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

sp_country_biotyp <- map(c("Delta", "Estuary", "Lagoon", "OpenCoast"), int_biotyp) %>%
  bind_cols() %>%
  Filter(function(x) !all(x == 0), .)

PUs <- PUs %>%
  dplyr::select(!(starts_with("Sp_"))) %>%
  dplyr::select(!c("country", "Delta", "Estuary", "Lagoon", "OpenCoast")) %>%
  add_column(sp_country_biotyp) %>%
  relocate(geometry, .before = ID)

saveRDS(PUs,
        "Results/RDS/PUs_04_mangroves_cc_IUCN_split_by_country_and_biotyp.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()