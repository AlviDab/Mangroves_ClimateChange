## load packages
library(sf)
## Linking to GEOS 3.5.1, GDAL 2.2.2, proj.4 4.9.2
pacman::p_load(microbenchmark, future.apply)
library(rnaturalearth)
library(lwgeom)
library(tidyverse)
library(parallelly)

## Linking to liblwgeom 2.5.0dev r16016, GEOS 3.5.1, proj.4 4.9.2
library(testthat)

PUs <- readRDS("Data/PUs/PUs_EPSG4326_crop.rds")

## fetch example data and clean it
data <- ne_countries(scale = 50, returnclass ="sf") %>%
  dplyr::select("continent") %>%
  st_wrap_dateline()

plot(data)
plot(PUs)

ncores <- parallelly::availableCores() - 10

PUs_parallel <- PUs %>%
  split(seq(1:ncores))

benchmark_data <- microbenchmark(
  # standard = st_intersection(data, PUs),
  parallel = {
    plan(multisession, workers = ncores)

    future_lapply(PUs_parallel, function(x) {
      st_intersection(data, x)
    })

    plan(sequential)},
  times = 3L,
  unit = "s")

print(benchmark_data)
