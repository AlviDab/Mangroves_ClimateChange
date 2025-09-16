#Author: Alvise Dabalà
#Date: 03/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

CC_direction <- "mean"

PUs <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/country_and_biotyp_targets_area/",
                      CC_direction, "/solution_0.05_", CC_direction, ".rds"))

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(c("mean", "landward", "seaward"),
           .options = furrr_options(seed = TRUE),
           function(CC_direction) {
             dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

             source("Code/Functions/f_create_worldmap.r")
             world_map <- f_worldmap()

             plot_map <- ggplot() +
               geom_sf(data = world_map, fill = "grey60",
                       colour = "grey60",
                       linewidth = 0.001) +
               geom_sf(data = PUs,
                       aes(fill = !!sym(paste0("Prob_gain_stability_", CC_direction)),
                           colour = !!sym(paste0("Prob_gain_stability_", CC_direction))),
                       linewidth = 0.01) +
               scale_colour_viridis_c(option = "C", guide = 'none') +
               scale_fill_viridis_c(option = "C") +
               guides(fill = guide_colourbar(barwidth = 10,
                                             barheight = 0.5,
                                             title.position = "top",
                                             title = paste0("Probability of gain stability (", CC_direction, ")"),
                                             ticks.colour = "black")) +
               geom_sf(data = dat, fill = NA) +
               theme_minimal(base_size = 7) +
               theme(panel.grid.major = element_line(colour = "transparent"),
                     panel.background = element_blank(),
                     legend.position = "top",
                     legend.box = "vertical",
                     legend.key.size = unit(0.3, "cm")) +
               scale_x_continuous(expand = c(0, 0)) +
               scale_y_continuous(expand = c(0, 0)) +
               coord_sf(datum = NA)

             dir.create(paste0("Figures/Country/Layers/01_PUs_CC/RDS"), recursive = TRUE)

             ggsave(plot = plot_map, paste0("Figures/Country/Layers/01_PUs_CC/PUs_CC_",
                                            CC_direction, ".pdf"),
                    dpi = 300, width = 18, height = 11, units = "cm")

             saveRDS(plot_map, paste0("Figures/Country/Layers/01_PUs_CC/RDS/PUs_CC_",
                                      CC_direction, ".rds"))
           })

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()