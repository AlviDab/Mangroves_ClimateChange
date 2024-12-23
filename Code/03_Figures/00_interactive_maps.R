#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, tmap)

prct <- 0.3
CC_direction <- "mean"

# ncores <- detectCores() - 6
#
# plan(multisession, workers = ncores)
#
# future_map(seq(0.05, 1, by = 0.05),
#            .options = furrr_options(seed = TRUE),
#            function(prct) {
#
#              map(c("landward", "seaward",
#                    "mean"
#              ), function(CC_direction) {

map(c("Country_and_biotyp", "biotyp"), function(split_group) {

  solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                             split_group,"/solution_prioritisation.rds"))

  solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                split_group, "/",
                                CC_direction, "/solution_",
                                as.character(prct), "_", CC_direction, ".rds"))

  dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

  source("Code/Functions/f_create_worldmap.r")
  world_map <- f_worldmap()

  sol <- solution_cc %>%
    mutate(solution_2 = solution$solution_1) %>%
    mutate(overlap = case_when(
      solution_1 == 1 & solution_2 == 1 ~ "Both plans",
      solution_1 == 0 & solution_2 == 1 ~ "Only climate-naïve",
      solution_1 == 1 & solution_2 == 0 ~ "Only climate-smart",
      solution_1 == 0 & solution_2 == 0 ~ "Not selected",
      .default = NA
    ))

  sol <- sol %>%
    st_transform(4326) %>%
    wdpar::st_repair_geometry()

  priority_sol <- sol %>%
    filter(priority == TRUE)

  tmap_mode("view")

  interactive_map <- tmap::tm_shape(sol) +
    tm_polygons(col = "overlap",
                palette = c("#F58300", "#CECECE", "#0F0247", "#26AFD1"),
                interactive = T,
                title = "Plans comparison"
    ) +
    tmap::tm_shape(priority_sol) +
    tm_borders(col = "black")

  dir.create(paste0("Figures/Country/00_interactive_maps/", split_group, "/RDS"), recursive = TRUE)

  tmap::tmap_save(tm = interactive_map, paste0("Figures/Country/00_interactive_maps/",
                                               split_group,"/overlap_",
                                               CC_direction, "_", prct, ".html"))

  saveRDS(interactive_map, paste0("Figures/Country/00_interactive_maps/",
                                  split_group,"/RDS/overlap_",
                                  CC_direction, "_", prct, ".rds"))
})
#   })
# })

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.