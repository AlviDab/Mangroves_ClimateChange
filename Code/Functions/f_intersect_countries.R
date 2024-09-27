#Author: Alvise Dabalà
#Date: 09/05/2024

f_int_countries <- function(PUs) {

  source("Code/Functions/f_create_worldmap.r")
  world_map <- f_worldmap(scale_map = "large")

  countries <- world_map %>%
    group_by(geounit) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf()

  intersection <- PUs %>%
    st_nearest_feature(countries)

  PUs <- PUs %>%
    mutate(country = countries$geounit[intersection])

  return(PUs)
}