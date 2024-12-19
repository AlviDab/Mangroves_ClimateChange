#Author: Alvise Dabalà
#Date: 09/05/2024

f_int_countries <- function(PUs) {

  source("Code/Functions/f_create_worldmap.r")
  world_map <- f_worldmap(scale_map = "large")

  world_map <- world_map %>%
    mutate(jurisdiction = case_when(
      type %in% c("Dependency", "Country") ~ name_long, #Add dependency
      sovereignt %in% c("France", "Netherlands") ~ name_long, #Add overseas territories
      .default = sovereignt
    ))

  a <- world_map %>%
    select(jurisdiction, type)

  countries <- world_map %>%
    group_by(jurisdiction) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf()

  intersection <- PUs %>%
    st_nearest_feature(countries)

  PUs <- PUs %>%
    mutate(country = countries$jurisdiction[intersection])

  return(PUs)
}
