#Author: Alvise Dabalà
#Date: 1107/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, biscale, pals)

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

                 #Make lists for each group of small PUs that intersect one large PUs
                 small_solution_noCC <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                       split_group,"/solution_prioritisation.rds")) %>%
                   mutate(type = "noCC")

                 int_sol_noCC <- solution_noCC %>%
                   st_intersects(small_solution_noCC %>%
                                   st_centroid(.))

                 small_solution_noCC <- int_sol_noCC %>%
                   map(function(elements_sol){
                     small_solution_noCC %>%
                       slice(elements_sol)
                   })

                 small_solution_CC <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                     split_group, "/",
                                                     CC_direction, "/solution_",
                                                     as.character(prct), "_", CC_direction, ".rds")) %>%
                   mutate(type = "CC")

                 int_sol_CC <- solution_CC %>%
                   st_intersects(small_solution_CC %>%
                                   st_centroid(.))

                 small_solution_CC <- int_sol_CC %>%
                   map(function(elements_sol){
                     small_solution_CC %>%
                       slice(elements_sol)
                   })

                 #Calculate the kappa for each group
                 kappa <- map(seq_along(small_solution_noCC),
                              function(index) {

                                if(nrow(small_solution_noCC[[index]]) > 0) {
                                  spatialplanr::splnr_get_kappaCorrData(list(small_solution_noCC[[index]],
                                                                             small_solution_CC[[index]]),
                                                                        c("CC", "noCC"))[[2]]
                                } else {
                                  NaN
                                }

                              }) %>%
                   unlist()

                 comparison_solution <- comparison_solution %>%
                   mutate(kappa = kappa)

                 dir.create(paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                                   split_group, "/",
                                   CC_direction), recursive = TRUE)

                 st_write(comparison_solution, paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                                                      split_group, "/",
                                                      CC_direction, "/Comparison_large_",
                                                      as.character(prct), "_", CC_direction, "_kappa.gpkg"))

                 comparison_solution_clean <- comparison_solution %>%
                   filter(!is.na(kappa))

                 biv_data <- bi_class(comparison_solution_clean,
                                      x = kappa,
                                      y = diff_perc_selection_CC_noCC,
                                      style = "quantile",
                                      dim = 3)

                 plot_map <- ggplot() +
                   geom_sf(data = world_map, fill = "grey60",
                           colour = "grey60",
                           linewidth = 0.001) +
                   geom_sf(data = biv_data,
                           aes(fill = bi_class),
                           colour = "black",
                           lwd = 0.0001) +
                   bi_scale_fill(pal = "DkBlue2", dim = 3) +
                   geom_sf(data = dat, fill = NA) +
                   theme_minimal(base_size = 7) +
                   theme(panel.grid.major = element_line(colour = "transparent"),
                         panel.background = element_blank(),
                         legend.position = "none") +
                   scale_x_continuous(expand = c(0, 0)) +
                   scale_y_continuous(expand = c(0, 0)) +
                   scale_alpha_continuous(range = c(0.8, 1)) +
                   coord_sf(datum = NA)

                 plot_legend <- bi_legend(pal = "DkBlue2",
                                          dim = 3,
                                          xlab = "Higher similarity",
                                          ylab = "Higher selection",
                                          size = 8,
                                          pad_width = 1.5)

                 finalPlot <- cowplot::ggdraw() +
                   cowplot::draw_plot(plot_legend, 0.7, 0.8, .3, .3) +
                   cowplot::draw_plot(plot_map, 0, 0, 1, 0.8)

                 dir.create(paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                                   split_group, "/RDS"),
                            recursive = TRUE)

                 ggsave(plot = finalPlot, paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                                                split_group,"/map_",
                                                CC_direction, "_", prct, ".pdf"),
                        dpi = 300, width = 18, height = 11, units = "cm")

                 saveRDS(finalPlot, paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
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