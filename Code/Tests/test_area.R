pacman::p_load(sf, tidyverse, reprex)

#Mollweide projection
moll_proj <- "ESRI:54009"

#Open the GMW map
biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp")

#GMW map
gmw <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp")

#GMW area
gmw_area <- gmw %>%
  st_area() %>%
  sum()

#Make biotyp valid
biotyp_valid <- biotyp %>%
  st_make_valid()

#Baseline area
biotyp_area <- biotyp_valid %>%
  st_area() %>%
  sum()

#crop biotyp
biotyp_crop <- biotyp %>%
  st_crop(xmax = -30, xmin = -40, ymax = 0, ymin = -15)

##############
#NO PROJECTION

#Area using spherical calculations
sf_use_s2(TRUE)

biotyp_area <- biotyp_crop %>%
  st_area() %>%
  sum()

#Turning s2 off
sf_use_s2(FALSE)

biotyp_area_planar <- biotyp_crop %>%
  st_area() %>%
  sum()

####################
#PROJECTED MOLLWEIDE

#Project to Mollweide
biotyp_proj <- biotyp_crop %>%
  st_make_valid() %>%
  st_transform(moll_proj) %>%
  st_make_valid()

#Area using planar projection
sf_use_s2(TRUE)

biotyp_proj_area <- biotyp_proj %>%
  st_area() %>%
  sum()

#Turning s2 off
sf_use_s2(FALSE)

biotyp_proj_area_planar <- biotyp_proj %>%
  st_area() %>%
  sum()

########################################
#PROJECTED MOLLWEIDE INCREASED PRECISION

#Project to Mollweide with increase in precision
biotyp_proj_1 <- biotyp_crop %>%
  st_set_precision(2500) %>%
  st_make_valid() %>%
  st_transform(moll_proj) %>%
  st_set_precision(2500) %>%
  st_make_valid()

#Area using planar projection
sf_use_s2(TRUE)

biotyp_proj_1_area <- biotyp_proj_1 %>%
  st_area() %>%
  sum()

#Turning s2 off
sf_use_s2(FALSE)

biotyp_proj_1_area_planar <- biotyp_proj_10000 %>%
  st_area() %>%
  sum()

######
#WDPAR

#Project to Mollweide and make valid using wdpar function
biotyp_proj_wdpar <- biotyp_crop %>%
  wdpar::st_repair_geometry() %>%
  st_transform(moll_proj) %>%
  wdpar::st_repair_geometry()

biotyp_proj_wdpar_area <- biotyp_proj_wdpar %>%
  st_area() %>%
  sum()

#Area using planar calculation of the projected map
sf_use_s2(TRUE)

biotyp_proj_area <- biotyp_proj %>%
  st_area() %>%
  sum()