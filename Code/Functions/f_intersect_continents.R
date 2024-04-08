#Author: Alvise Dabalà
#Date: 08/04/2024

f_int_continents <- function(PUs) {

  source("Code/Functions/f_create_worldmap.r")
  world_map <- f_worldmap(scale_map = "small")

  continents <- world_map %>%
    group_by(continent) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf()

  intersection <- PUs %>%
    st_nearest_feature(continents)

  PUs <- PUs %>%
    mutate(continent = continents$continent[intersection])

  return(PUs)
}