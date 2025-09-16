# Author: Alvise Dabalà
# Date: 18/03/2024
# Description: Barplot to compare the amount of area selected in climate-smart vs
#             climate-naïve prioritisation

################################################################################

# Libraries
pacman::p_load(tidyverse, sf, parallel, furrr, purrr)

# Select the parameters
prct <- 0.3
CC_direction <- "mean"

# ncores <- detectCores() - 2
#
# plan(multisession, workers = ncores)
#
# future_map(seq(0.05, 1, by = 0.05),
#            function(prct) {
# map(c("landward", "seaward",
#       "mean"), function(CC_direction) {


map(c("targets_30", "targets_area", "targets_area_1000PU"), function(targets) {
  map(c("country_and_biotyp", "biotyp"), function(split_group) {
    # load solutions
    solution <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/01_prioritisation/",
      split_group, "_", targets, "/solution_prioritisation.rds"
    ))

    solution_cc <- readRDS(paste0(
      "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
      split_group, "_", targets, "/",
      CC_direction, "/solution_",
      as.character(prct), "_", CC_direction, ".rds"
    ))


    # area climate-naïve
    area_cn <- solution %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      group_by(solution_1) %>%
      summarise(area_km2 = sum(area_km2)) %>%
      mutate(prct_area = area_km2 / sum(area_km2) * 100) %>%
      mutate(type = "Climate-naïve")

    # area climate-smart
    area_cs <- solution_cc %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      group_by(solution_1) %>%
      summarise(area_km2 = sum(area_km2)) %>%
      mutate(prct_area = area_km2 / sum(area_km2) * 100) %>%
      mutate(type = "Climate-smart")

    # total area
    total_area <- area_cs %>%
      summarise(sum(area_km2)) %>%
      as.numeric()

    # percentage difference
    diff <- area_cs$area_km2[2] - area_cn$area_km2[2]

    # create folder
    dir.create(paste0(
      "Figures/Country/01_area/",
      split_group, "_", targets, "/RDS"
    ), recursive = TRUE)

    # save text with percentage increase
    writeLines(
      paste0("Percentage increase in total area climate-smart prioritisation compared to climate naive = ", as.character(diff / area_cn$area_km2[2] * 100), "%"),
      paste0(
        "Figures/Country/01_area/",
        split_group, "_", targets, "/barplot_area_",
        CC_direction, "_", prct, "_prct_increase_area.txt"
      )
    )

    # barplot
    area <- rbind(area_cn, area_cs) %>%
      filter(solution_1 == 1)

    barplot_area <- ggplot(data = area) +
      geom_bar(aes(y = type, x = area_km2, fill = type), stat = "identity") +
      scale_fill_manual(
        values = c("#0F0247", "#26AFD1"),
        name = ""
      ) +
      geom_text(
        aes(
          label = scales::percent(area_km2 / total_area,
            accuracy = 0.01
          ),
          y = type, x = area_km2
        ),
        size = 6 * (5 / 14),
        colour = "white",
        vjust = 0.5, hjust = 1
      ) +
      theme_classic() +
      xlab(expression("Area km"^2)) +
      ylab("") +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        text = element_text(size = 8),
        axis.text = element_text(size = 7)
      ) +
      scale_x_continuous(limits = c(0, max(area$area_km2) * 1.1), expand = c(0, 0))

    # save figure
    ggsave(
      plot = barplot_area, paste0(
        "Figures/Country/01_area/",
        split_group, "_", targets, "/barplot_area_",
        CC_direction, "_", prct, ".pdf"
      ),
      dpi = 300, width = 12, height = 8, units = "cm"
    )

    saveRDS(barplot_area, paste0(
      "Figures/Country/01_area/",
      split_group, "_", targets, "/RDS/barplot_area_",
      CC_direction, "_", prct, ".rds"
    ))
  })
})
# })

# Return to sequential processing
plan(sequential)

# Clear memory
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() # free up memrory and report the memory usage.
.rs.restartR() # restart R session
