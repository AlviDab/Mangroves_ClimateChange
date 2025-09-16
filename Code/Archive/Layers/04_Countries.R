#Author: Alvise Dabalà
#Date: 30/05/2024

pacman::p_load(tidyverse, sf)

dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

PUs <- readRDS("Results/RDS/prioritisation/Country/02_prioritisation_CC/country_and_biotyp_targets_area/mean/solution_0.05_mean.rds")

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

ggplot() +
  geom_sf(data = world_map, fill = "grey60",
          colour = "black",
          linewidth = 0.001) +
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

dir.create("Figures/Country/Layers/04_countries/countries.pdf")

ggsave("Figures/Country/Layers/04_countries/Countries_distribution.pdf",
       width = 18, height = 10, dpi = 300, units = "cm")
