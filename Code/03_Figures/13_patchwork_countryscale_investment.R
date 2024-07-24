#Author: Alvise Dabalà
#Date: 24/07/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

# ncores <- detectCores() - 2
#
# plan(multisession, workers = ncores)
#
# future_map(seq(0.05, 0.3, by = 0.05),
#            .options = furrr_options(seed = TRUE),
#            function(prct) {
prct <- 0.3

map(c("country_and_biotyp",
      "biotyp"), function(split_group) {

        plot_legend <- readRDS(paste0("Figures/Country/08a_plot_diff_area_resilience/seaward/RDS/diff_area_resilience_seaward_by_country_",
                                      prct, "_legend.rds")) +
          theme(legend.spacing.y = unit(0, "cm"))

        plot_lw <- readRDS(paste0("Figures/Country/08a_plot_diff_area_resilience/landward/RDS/diff_area_resilience_landward_by_country_", prct, ".rds"))

        plot_sw <- readRDS(paste0("Figures/Country/08a_plot_diff_area_resilience/seaward/RDS/diff_area_resilience_seaward_by_country_", prct, ".rds"))

        linear_lw <- readRDS(paste0("Figures/Country/12_linear_investment/RDS/map_landward.rds"))

        linear_sw <- readRDS(paste0("Figures/Country/12_linear_investment/RDS/map_seaward.rds"))

        theme_txt <- theme(title = element_text(size = 11,
                                                face = 'bold'),
                           text = element_text(size = 9),
                           axis.text = element_text(size = 10,
                                                    face = 'bold'))

        figure_02_a <- ((plot_lw +
                           ggtitle("Landward selection") +
                           theme(legend.position = 'none') +
                           theme_txt) /
                          (plot_sw +
                             ggtitle("Seaward selection") +
                             theme(legend.position = 'none') +
                             theme_txt)
                        ) &
          theme(plot.tag = element_text(face = 'bold'),
                title = element_text(size = 11,
                                     face = 'bold'),
                text = element_text(size = 9),
                axis.text = element_text(size = 10)) &
          plot_annotation(tag_levels = list(c('a', 'b')))

        figure_02_b <- (linear_lw | linear_sw) +
          plot_layout(guides = 'collect',
                      axes = "collect") &
          theme(plot.tag = element_text(face = 'bold'),
                title = element_text(size = 10),
                text = element_text(size = 9),
                axis.text = element_text(size = 10,
                                         face = 'plain'),
                legend.position = 'top',
                legend.text = element_text(size = 9)) &
          plot_annotation(tag_levels = list(c('c', 'd')))

        layout <- c(
          patchwork::area(t = 1, l = 1, b = 30, r = 60),
          patchwork::area(t = 35, l = 1, b = 185, r = 60),
          patchwork::area(t = 190, l = 1, b = 250, r = 60)
        )

        figure_02 <- plot_legend + figure_02_a + figure_02_b +
          plot_layout(design = layout)

        dir.create(paste0("Figures/Country/13_patchwork_countryscale_investment/",
                          split_group), recursive = TRUE)

        ggsave(plot = figure_02, paste0("Figures/Country/13_patchwork_countryscale_investment/",
                                        split_group,"/patchwork_countryscale_investment_", prct, ".pdf"),
               dpi = 300, width = 18, height = 27, units = "cm")

        ggsave(plot = figure_02, paste0("Figures/Country/13_patchwork_countryscale_investment/",
                                        split_group,"/patchwork_countryscale_investment_", prct, ".jpg"),
               dpi = 300, width = 18, height = 27, units = "cm")
      })
# })

# plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
