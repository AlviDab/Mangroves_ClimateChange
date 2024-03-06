#Author: Alvise Dabalà
#Date: 18/04/2023

pacman::p_load(sf, tidyverse)

moll_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

mangroves_distribution <- st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>% 
  st_transform(moll_proj) #%>% 
  # st_make_valid()

#sf::sf_use_s2(FALSE)

dir.create("Results/RDS/", recursive = TRUE)
saveRDS(mangroves_distribution, "Results/RDS/mangroves_distribution_mollweide.rds")

source("Code/Functions/fCreate_PUs.R")

PUs <- fCreate_PUs(mangroves_distribution, 655) #~0.25° at the equator, apothem of 13.5 km

dir.create("Results/gpkg", recursive = TRUE)
saveRDS(PUs, "Results/RDS/00_PUs_mollweide.rds")
st_write(PUs, "Results/gpkg/00_PUs_mollweide.gpkg")

st_area(PUs[1,])

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()