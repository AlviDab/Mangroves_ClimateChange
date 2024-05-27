#Author: Alvise Dabalà
#Date: 24/05/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

map(c("landward", "seaward", "mean"), function(CC_direction) {

  prct_seq <- c(seq(0.05, 0.3, by = 0.05))

  future_map(prct_seq,
             .options = furrr_options(seed = TRUE),
             function(prct) {

               map(c("country_and_biotyp", "biotyp"), function(split_group) {

                 solution_noCC <- readRDS(paste0("Figures/Country/04_map_large/",
                                                 split_group, "/RDS/large_PUs_sol_noCC_0.rds"))

                 solution_CC <- readRDS(paste0("Figures/Country/04_map_large/",
                                               split_group, "/RDS/large_PUs_sol_",
                                               CC_direction, "_", prct, ".rds"))

                 dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

                 comparison_solution <- solution_CC %>%
                   mutate(diff_perc_selection_CC_noCC =
                            ((selected_MangroveArea_km2/MangroveArea_km2) -
                               (solution_noCC$selected_MangroveArea_km2/solution_noCC$MangroveArea_km2)
                             ))

                 comparison_solution_centroid <- comparison_solution %>%
                   st_centroid(.)

                 plot_map <- ggplot() +
                   geom_sf(data = world_map, fill = "grey60",
                           colour = "grey60",
                           linewidth = 0.001) +
                   geom_sf(data = comparison_solution,
                           aes(fill = diff_perc_selection_CC_noCC),
                           colour = "black",
                           lwd = 0.0001) +
                   scale_fill_gradient2(low = "#8c1e36", mid = "#f2f8ca", high = "#406fa2") +
                   guides(fill = guide_colourbar(barwidth = 10,
                                                 barheight = 0.5,
                                                 title.position = "top",
                                                 title = "Difference in the proportion of mangroves selected",
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
                   scale_alpha_continuous(range = c(0.8, 1)) +
                   coord_sf(datum = NA)

                 dir.create(paste0("Figures/Country/04a_map_large_comparison/", split_group, "/RDS"), recursive = TRUE)

                 ggsave(plot = plot_map, paste0("Figures/Country/04a_map_large_comparison/",
                                                split_group,"/map_",
                                                CC_direction, "_", prct, ".pdf"),
                        dpi = 300, width = 18, height = 11, units = "cm")

                 saveRDS(plot_map, paste0("Figures/Country/04a_map_large_comparison/",
                                          split_group, "/RDS/map_",
                                          CC_direction, "_", prct, ".rds"))
               })
             })
})

plan(sequential)

#Description of the figures
writeLines("")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()