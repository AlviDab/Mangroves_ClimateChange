#Author: Alvise Dabalà
#Date: 11/07/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, biscale, irr)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

map(c("landward", "seaward"
      #, "mean"
), function(CC_direction) {

  # prct_seq <- c(seq(#0.05,
  #   0.3, by = 0.05))

  # future_map(prct_seq,
  #            .options = furrr_options(seed = TRUE),
  #            function(prct) {
  prct <- 0.3

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
    kappa_vct <- map(seq_along(small_solution_noCC),
                     function(index) {

                       # NaN if no PUs intersect the large PU
                       if(nrow(small_solution_noCC[[index]]) > 0) {
                         spatialplanr::splnr_get_kappaCorrData(list(small_solution_noCC[[index]],
                                                                    small_solution_CC[[index]]),
                                                               c("CC", "noCC"))[[2]]
                       } else {
                         "Empty" #If there is no intersection with small planning units
                       }

                     }) %>%
      unlist()

    comparison_solution <- comparison_solution %>%
      filter(kappa != "Empty") %>%
      mutate(kappa = as.numeric(kappa_vct))

    dir.create(paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                      split_group, "/",
                      CC_direction), recursive = TRUE)

    st_write(comparison_solution, paste0("Results/gpkg/prioritisation/Country/03_comparison/",
                                         split_group, "/",
                                         CC_direction, "/Comparison_large_",
                                         as.character(prct), "_", CC_direction, "_kappa.gpkg"),
             append = TRUE)

    comparison_solution_clean <- comparison_solution %>%
      filter(!is.na(kappa))

    ## calculate breaks
    breaks_x <- c(-1, 0, 0.33, 0.66, 1)
    breaks_y <- c(-1, -0.5, 0, 0.5, 1)

    ## cut data
    comparison_solution_clean <- comparison_solution_clean %>%
      mutate(kappa_bin = cut(kappa,
                             breaks = breaks_x,
                             include.lowest = TRUE))

    comparison_solution_clean <- comparison_solution_clean %>%
      mutate(diff_perc_selection_CC_noCC_bin = cut(diff_perc_selection_CC_noCC,
                                                   breaks = breaks_x,
                                                   include.lowest = TRUE))

    biv_data <- bi_class(comparison_solution_clean,
                         x = kappa_bin,
                         y = diff_perc_selection_CC_noCC_bin,
                         dim = 4)

    plot_map <- ggplot() +
      geom_sf(data = world_map, fill = "grey60",
              colour = "grey60",
              linewidth = 0.001) +
      geom_sf(data = solution_noCC,
              fill = "white",
              colour = "black",
              lwd = 0.0001) +
      geom_sf(data = biv_data,
              aes(fill = bi_class),
              colour = "black",
              lwd = 0.0001) +
      bi_scale_fill(pal = "DkBlue2", dim = 3) +
      geom_sf(data = dat, fill = NA) +
      theme_minimal(base_size = 6) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.background = element_blank(),
            legend.position = "none") +
      coord_sf(datum = NA)

    legend_breaks <- list()

    legend_breaks$bi_x <- breaks_x %>%
      round(digits = 2)
    legend_breaks$bi_y <- breaks_y

    plot_legend <- bi_legend(pal = "DkBlue2",
                             dim = 4,
                             xlab = "Cohen's kappa",
                             ylab = "Proportion selected",
                             size = 8,
                             breaks = legend_breaks,
                             pad_width = 0.2,
                             arrows = FALSE)

    finalPlot <- cowplot::ggdraw() +
      cowplot::draw_plot(plot_legend, 0.75, 0.7, .3, .3) +
      cowplot::draw_plot(plot_map, 0, 0, 1, 0.8)

    dir.create(paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                      split_group, "/RDS"),
               recursive = TRUE)

    ggsave(plot = finalPlot, paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                                    split_group,"/map_",
                                    CC_direction, "_", prct, ".pdf"),
           dpi = 300, width = 18, height = 11, units = "cm")

    saveRDS(plot_legend, paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                                    split_group,"/RDS/legend.rds"))

    saveRDS(plot_map, paste0("Figures/Country/04b_map_large_comparison_area_kappa/",
                              split_group, "/RDS/map_",
                              CC_direction, "_", prct, ".rds"))
  })
})
# })

plan(sequential)

#Description of the figures
writeLines("")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()