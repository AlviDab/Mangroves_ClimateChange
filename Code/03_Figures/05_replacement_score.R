#Author: Alvise Dabalà
#Date: 04/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, ggnewscale)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

future_map(c("country_and_biotyp", "biotyp"),
           .options = furrr_options(seed = TRUE),
           function(file_name) {

             replacement_score <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                 file_name,
                                                 "/replacement_score.rds"))

             irreplaceable <- replacement_score %>%
               mutate(type = case_when(is.infinite(rc) ~ "Irreplaceable",
                                       .default = "Not irreplaceable")) %>%
               mutate(type = as.factor(type))

             replacement_score <- replacement_score %>%
               filter(!is.infinite(rc))

             plot_replacement_score <- ggplot() +
               geom_sf(data = world_map, fill = "grey60",
                       colour = "grey60",
                       linewidth = 0.001) +
               geom_sf(data = irreplaceable,
                       aes(fill = type,
                           colour = type),
                       colour = "transparent",
                       linewidth = 0.01) +
               scale_fill_manual(breaks = c("Irreplaceable"), values = c("red", "none"),
                                 name = "") +
               scale_colour_manual(breaks = c("Irreplaceable"), values = c("red", "none"),
                                   guide = "none") +
               new_scale_fill() +
               new_scale_colour() +
               geom_sf(data = replacement_score,
                       aes(fill = log10(rc + 1),
                           colour = log10(rc + 1)),
                       linewidth = 0.01) +
               scale_colour_viridis_c(option = "G", na.value = "red",
                                      guide = "none") +
               scale_fill_viridis_c(option = "G", na.value = "red",
                                    guide = guide_colourbar(barwidth = 10,
                                                            barheight = 0.5,
                                                            title.position = "top",
                                                            title = expression("log"[10]*"(Replacement score)"),
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

             dir.create(paste0("Figures/Country/07_replacement_score/no_CC/", file_name, "/RDS"), recursive = TRUE)

             ggsave(plot = plot_replacement_score, paste0("Figures/Country/07_replacement_score/no_CC/",
                                                          file_name, "/replacement_score", ".pdf"),
                    dpi = 300, width = 18, height = 11, units = "cm")

             saveRDS(plot_replacement_score, paste0("Figures/Country/07_replacement_score/no_CC/",
                                                    file_name, "/RDS/replacement_score", ".rds"))
           })

plan(sequential)

#Description of the figures
writeLines("In the CC folder there are the replacement score maps for the climate-smart prioritisation and in the no_CC those for the climate-naive.

In the 'biotyp' folder are all the results for prioritisations that include conservation features not split by country.
Viceversa in 'country_and_biotyp' the conservation features were split by country.

The value reported in the name of each file is the percentage used as a tradeoff to select climate-priority areas for each conservation feature (more on the method 'climate-priority areas' in Buenafe et al. 2023 - https://doi.org/10.1002/eap.2852).

The folder 'mean' indicate that I am using a mean value of landward and seaward change in the prioritisation.",
           "Figures/08_replacement_score/info.txt")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()