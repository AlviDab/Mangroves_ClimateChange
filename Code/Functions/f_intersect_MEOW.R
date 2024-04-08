#Author: Alvise Dabalà
#Date: 08/04/2024

f_int_MEOW <- function(PUs, type = "ECOREGION") {

  moll_proj <- "ESRI:54009"

  MEOW <- read_sf("Data/MEOW/Marine_Ecoregions_Of_the_World__MEOW_.shp") %>%
    st_transform(moll_proj) %>%
    st_make_valid()

  intersection <- PUs %>%
    st_centroid() %>% #so that it intersect only one ecoregion
    st_nearest_feature(MEOW)

  PUs <- PUs %>%
    cbind(provinces[intersection, type] %>%
             st_drop_geometry())

  return(PUs)
}