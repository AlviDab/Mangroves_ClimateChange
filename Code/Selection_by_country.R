pacman::p_load(tidyverse, sf)

solution_nocc <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/biotyp/solution_prioritisation.rds"))

source("Code/Functions/f_intersect_countries.R")

#Intersect countries
PUs_country <- solution_nocc %>%
  f_int_countries() %>%
  mutate(country = case_when(country == "France" ~ "French Guiana",
                             .default = country) %>%
           gsub(" ", "_", .))

area_selected <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_1 == TRUE) %>%
  group_by(country) %>%
  summarise(area_selected = sum(area_km2)) %>%
  arrange(desc(area_selected))

area_not_selected <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_1 == FALSE) %>%
  group_by(country) %>%
  summarise(area_not_selected = sum(area_km2)) %>%
  arrange(desc(area_not_selected))

prct_area_selected <- area_selected %>%
  left_join(area_not_selected, by = "country") %>%
  mutate(prct_area_selected = area_selected/(area_not_selected + area_selected)) %>%
  arrange(desc(prct_area_selected))

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()