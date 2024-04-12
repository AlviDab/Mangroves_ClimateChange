#Author: Alvise Dabalà
#Date: 22/03/2024

pacman::p_load(tidyverse, sf, patchwork, purrr)

figures_biotyp <- list.files(path = "./Figures/09_plot_percarea_CCratio/by_ecoregion/biotyp/RDS",
                             pattern = NULL,
                             all.files = FALSE,
                             full.names = TRUE) %>%
  map(~readRDS(.))

 figures_MEOW_biotyp <- list.files(path = "./Figures/09_plot_percarea_CCratio/by_ecoregion/MEOW_and_biotyp/RDS",
                                  pattern = NULL,
                                  all.files = FALSE,
                                  full.names = TRUE) %>%
  map(~readRDS(.))

(wrap_plots(figures_biotyp, ncol = 1) | wrap_plots(figures_MEOW_biotyp, ncol = 1)) + plot_layout(guides = 'collect', axes = 'collect') &
   guides(
     fill = guide_legend(
       nrow = 5))
