#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 1, by = 0.05),
           .options = furrr_options(seed = TRUE),
           function(prct) {

             map(c("landward", "seaward",
                   "mean"), function(CC_direction) {

                     map(c("country_and_biotyp", "biotyp"), function(split_group) {

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

                       dir.create(paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                                         split_group, "/",
                                         CC_direction), recursive = T)

                       st_write(sol %>%
                                  dplyr::select(!starts_with("Sp_")),
                                paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                                split_group, "/",
                                CC_direction, "/Comparison_",
                                as.character(prct), "_", CC_direction, ".gpkg"))

                       plot_overlap <- ggplot() +
                         geom_sf(data = world_map, fill = "grey60",
                                 colour = "grey60",
                                 linewidth = 0.001) +
                         geom_sf(data = sol,
                                 aes(fill = overlap,
                                     colour = priority),
                                 linewidth = 0.01) +
                         scale_fill_manual(values = c("#F58300", "#CECECE", "#0F0247", "#26AFD1"),
                                           name = "") +
                         scale_colour_manual(values = c("transparent", "black"),
                                             labels = c("Not climate-priority areas",
                                                        "Climate-priority areas"),
                                             name = "") +
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

                       dir.create(paste0("Figures/Country/01_map_differences/", split_group, "/RDS"), recursive = TRUE)

                       ggsave(plot = plot_overlap, paste0("Figures/Country/01_map_differences/",
                                                          split_group,"/overlap_",
                                                          CC_direction, "_", prct, ".pdf"),
                              dpi = 300, width = 18, height = 11, units = "cm")

                       saveRDS(plot_overlap, paste0("Figures/Country/01_map_differences/",
                                                    split_group, "/RDS/overlap_",
                                                    CC_direction, "_", prct, ".rds"))
                     })
                   })
           })

plan(sequential)

#Description of the figures
writeLines("The figure is a map of the planning units that overlap the global mangrove distribution. There is a comparison of the selection using climate-smart and climate-naive methods.

'mean' means that I am using a mean value of landward and seaward change in the prioritisation.
'landward' means that I am using landward change in the prioritisation.
'seaward' means that I am using seaward change in the prioritisation.

The value reported is the percentage used as a tradeoff to select climate-priority areas for each conservation feature (more on the method 'climate-priority areas' in Buenafe et al. 2023 - https://doi.org/10.1002/eap.2852).

In the figures:

# Fill
- both plans → the planning unit was selected in both climate-smart and climate-naive plans;
- only climate-smart → planning unit was selected only in the climate-smart plan;
- only climate-naive → planning unit was selected only in the climate-naive plan;
- not selected → the planning units was never selected.

# Border of the planning unit
- black: planning unit locked-in as climate-priority area
- transparent: planning unit not locked-in",
           "Figures/Country/01_map_differences/info.txt")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
