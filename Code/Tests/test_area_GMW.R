pacman::p_load(sf, tidyverse, lwgeom)

#area GMW
gmw <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp")

#area using ellipsoidal approximation
gmw_area <- gmw %>%
  lwgeom::st_geod_area()

total_area_gmw <- gmw_area %>%
  sum() #Result: 145690900911 [m^2]

#area using spherical approximation
gmw_area_s2 <- gmw %>%
  st_area()

total_area_gmw_s2 <- gmw_area_s2 %>%
  sum() #Result: 146247317269 [m^2]

#area biotyp
biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp")

biotyp <- biotyp %>%
  st_make_valid()

#area using ellipsoidal approximation
biotyp_area <- biotyp %>%
  lwgeom::st_geod_area()

total_area_biotyp <- biotyp_area %>%
  sum() #Result: 145690902956 [m^2]

#area using spherical approximation
biotyp_area_s2 <- biotyp %>%
  st_area()

total_area_biotyp_s2 <- biotyp_area_s2 %>%
  sum() #Result: 146247315645 [m^2]
