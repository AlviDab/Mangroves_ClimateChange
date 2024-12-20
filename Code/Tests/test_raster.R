pacman::p_load(tidyverse, sf, terra, fasterize)

biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp")

biotyp <- biotyp[1:20,]

PUs <- rast(xmin = xmin(biotyp), ymin = -90,
            xmax = 180, ymax = 90, resolution = c(0.25, 0.25),
            crs = "WGS84")

PUs <- setValues(PUs, sample(x = 0:100, size = ncell(PUs), replace = T))

plot(PUs)

tictoc::tic()
biotyp_tiff <- biotyp %>%
  rasterize(PUs)
tictoc::toc()

plot(biotyp_tiff)
