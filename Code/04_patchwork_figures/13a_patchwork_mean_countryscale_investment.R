#Author: Alvise Dabalà
#Date: 20/08/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

# ncores <- detectCores() - 2
#
# plan(multisession, workers = ncores)
#
# future_map(seq(0.05, 0.3, by = 0.05),
#            .options = furrr_options(seed = TRUE),
#            function(prct) {
prct <- 0.3

plot_legend <- readRDS(paste0("Figures/Country/08a_plot_diff_area_resilience/seaward/RDS/diff_area_resilience_seaward_by_country_",
                              prct, "_legend.rds")) +
  theme(legend.spacing.y = unit(0, "cm"))

plot_mean <- readRDS(paste0("Figures/Country/08a_plot_diff_area_resilience/mean/RDS/diff_area_resilience_mean_by_country_", prct, ".rds")) +
  theme(legend.position = "none") +
  labs(tag = "a")

linear <- readRDS(paste0("Figures/Country/12_linear_investment/RDS/linear_investment_mean.rds")) +
  labs(tag = "b")

theme_txt <- theme(title = element_text(size = 11,
                                        face = 'bold'),
                   axis.title = element_text(size = 10#,
                                             #face = 'bold')
                                             ))

layout <- c(
  patchwork::area(t = 1, l = 1, b = 30, r = 60),
  patchwork::area(t = 35, l = 1, b = 105, r = 60),
  patchwork::area(t = 110, l = 1, b = 170, r = 60)
)

figure_02 <- plot_legend + plot_mean + linear +
  plot_layout(design = layout) &
  theme_txt

dir.create("Figures/Country/13_patchwork_countryscale_investment/", recursive = TRUE)

ggsave(plot = figure_02, paste0("Figures/Country/13_patchwork_countryscale_investment/patchwork_countryscale_investment_mean_", prct, ".pdf"),
       dpi = 300, width = 18, height = 23, units = "cm")

ggsave(plot = figure_02, paste0("Figures/Country/13_patchwork_countryscale_investment/patchwork_countryscale_investment_mean_", prct, ".jpg"),
       dpi = 300, width = 18, height = 23, units = "cm")

# plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()