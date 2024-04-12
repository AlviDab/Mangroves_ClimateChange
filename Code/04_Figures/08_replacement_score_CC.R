#Author: Alvise Dabalà
#Date: 04/04/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, ggnewscale)

dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

map(seq(0.05, 0.3, by = 0.05),
    function(prct) {

      CC_direction <- "mean"

      map(c("MEOW_and_biotyp", "biotyp"), function(file_name) {

        replacement_score <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                            file_name, "/",
                                            CC_direction, "/replacement_score_",
                                            as.character(prct), "_", CC_direction, ".rds"))

        irreplaceable <- replacement_score %>%
          mutate(type = case_when(is.infinite(rc) ~ "Irreplaceable",
                                  .default = "Not irreplaceable")) %>%
          mutate(type = as.factor(type))

        replacement_score <- replacement_score %>%
          filter(rc == 0)

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

        dir.create(paste0("Figures/08_replacement_score/CC/", file_name,"/",
                          CC_direction, "/RDS"), recursive = TRUE)

        ggsave(plot = plot_replacement_score, paste0("Figures/08_replacement_score/CC/",
                                                     file_name, "/",
                                                     CC_direction, "/replacement_score_",
                                                     as.character(prct), "_", CC_direction, ".pdf"),
               dpi = 300, width = 18, height = 11, units = "cm")

        saveRDS(plot_replacement_score, paste0("Figures/08_replacement_score/CC/",
                                               file_name, "/",
                                               CC_direction, "/RDS/replacement_score_",
                                               as.character(prct), "_", CC_direction, ".rds"))
      })
    })

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()