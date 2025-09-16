# Author: Alvise Dabalà
# Date: 26/03/2024
# Description: This script creates maps of the large planning units with the prioritisation solutions overlaid.
#             It shows the percentage of selection of mangroves in each large planning unit.

###################################################################################

# Load packages
pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

# Set parameters
targets <- "targets_area"

# Set up parallel processing
ncores <- detectCores() - 2
set.seed(123)
plan(multisession, workers = ncores)

# Load functions and data
source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

# Load large planning units
PUs_large <- readRDS("Results/RDS/PUs_00_large_mollweide.rds")
rownames(PUs_large) <- 1:nrow(PUs_large)

# Add cellID to large PUs
PUs_large <- PUs_large %>%
  mutate(cellID = row.names(.))

future_map(c("noCC", "landward", "seaward", "mean"), function(CC_direction) {
  if (CC_direction == "noCC") {
    prct <- 0
  } else {
    # prct_seq <- c(seq(0.05, 1, by = 0.05))
    prct <- 0.3
  }

  #   map(prct_seq,
  #              .options = furrr_options(seed = TRUE),
  #              function(prct) {

  map(c("country_and_biotyp", "biotyp"), function(split_group) {
    # Load solutions
    if (CC_direction == "noCC") {
      solution <- readRDS(paste0(
        "Results/RDS/prioritisation/Country/01_prioritisation/",
        split_group, "_", targets, "/solution_prioritisation.rds"
      ))
    } else {
      solution <- readRDS(paste0(
        "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
        split_group, "_", targets, "/",
        CC_direction, "/solution_",
        as.character(prct), "_", CC_direction, ".rds"
      ))
    }

    # Get world boundary
    dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

    # find the intersection between the centroids of the solution and the large planning units
    intersection <- PUs_large %>%
      st_intersects(st_centroid(solution),
        sparse = TRUE
      )

    # select only the rows that intersect the large planning unit and
    # calculate their mean value
    large_sol <- map(seq_along(intersection), function(intersection_index) {
      if (length(intersection[[intersection_index]]) == 0) {
      } else {
        mean_selected_solution <- solution %>%
          slice(intersection[[intersection_index]]) %>%
          select(!c(ID, cellID)) %>%
          mutate(nPUs = nrow(.)) %>%
          summarise(
            across(!contains("Sp") & !contains("km2") & where(is.numeric), ~ mean(.x)),
            across(contains("km2") & where(is.numeric), ~ sum(.x))
          )

        # Calculate the selected mangrove area within the large planning unit
        selected_mangrove_area <- solution %>%
          slice(intersection[[intersection_index]]) %>%
          st_drop_geometry() %>%
          filter(solution_1 == 1) %>%
          summarise(selected_MangroveArea_km2 = sum(MangroveArea_km2)) %>%
          dplyr::select(selected_MangroveArea_km2)

        # Add the selected mangrove area to the mean selected solution if there are selected PUs
        if (nrow(selected_mangrove_area) > 0) {
          mean_selected_solution <- mean_selected_solution %>%
            add_column(selected_mangrove_area)
        }

        # Add the percentage of selection of mangrove area within the large planning unit
        PUs_large <- PUs_large %>%
          slice(intersection_index) %>%
          cbind(mean_selected_solution %>%
            st_drop_geometry(.))

        return(PUs_large)
      }
    }) %>%
      bind_rows()

    # Create directory if it does not exist
    dir.create(paste0("Figures/Country/03_map_large/", split_group, "_", targets, "/RDS"), recursive = TRUE)

    # Save the large planning units with the solution
    saveRDS(large_sol, paste0(
      "Figures/Country/03_map_large/",
      split_group, "_", targets, "/RDS/large_PUs_sol_",
      CC_direction, "_", prct, ".rds"
    ))

    # Plot the map
    plot_map <- ggplot() +
      geom_sf(
        data = world_map, fill = "grey60",
        colour = "grey60",
        linewidth = 0.001
      ) +
      geom_sf(
        data = large_sol,
        aes(fill = solution_1),
        colour = "black",
        linewidth = 0.01
      ) +
      scale_fill_viridis_c(option = "D") +
      guides(fill = guide_colourbar(
        barwidth = 10,
        barheight = 0.5,
        title.position = "top",
        title = "Percentage of selection (%)",
        ticks.colour = "black"
      )) +
      geom_sf(data = dat, fill = NA) +
      theme_minimal(base_size = 7) +
      theme(
        panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.position = "top",
        legend.box = "vertical",
        legend.key.size = unit(0.3, "cm")
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_sf(datum = NA)

    # Save the plot
    ggsave(
      plot = plot_map, paste0(
        "Figures/Country/03_map_large/",
        split_group, "_", targets, "/map_",
        CC_direction, "_", prct, ".pdf"
      ),
      dpi = 300, width = 18, height = 11, units = "cm"
    )

    saveRDS(plot_map, paste0(
      "Figures/Country/03_map_large/",
      split_group, "_", targets, "/RDS/map_",
      CC_direction, "_", prct, ".rds"
    ))
  })
})
# })

# Turn off parallel processing
plan(sequential)

# Clear environment
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() # free up memory and report the memory usage.
.rs.restartR()
