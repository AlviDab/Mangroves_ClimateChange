#Author: Alvise Dabalà
#Date: 08/04/2024

f_int_subregion <- function(PUs) {

  source("Code/Functions/f_create_worldmap.r")
  world_map <- f_worldmap(scale_map = "small")

  subregions <- world_map %>%
    group_by(subregion) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf()

  intersection <- PUs %>%
    st_nearest_feature(subregions)

  PUs <- PUs %>%
    mutate(subregion = subregions$subregion[intersection])

  return(PUs)
}