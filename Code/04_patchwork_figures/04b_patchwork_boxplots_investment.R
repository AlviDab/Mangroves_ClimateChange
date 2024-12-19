#Author: Alvise Dabalà
#Date: 20/08/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

prct <- 0.3

fit_global_mean <- readRDS("Figures/Country/10_linear_investment/RDS/fit_global_mean.rds")
fit_country_mean <- readRDS("Figures/Country/10_linear_investment/RDS/fit_country_mean.rds")

resilience_boxplot <- readRDS(paste0("Figures/Country/03a_resilience/RDS/boxplot_resilience_",
                                     CC_direction, "_", prct, ".rds")) +
  theme(legend.position = "none")

area_boxplot <- readRDS(paste0("Figures/Country/02a_area/RDS/boxplot_area_",
                               CC_direction, "_", prct, ".rds")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

linear <- readRDS(paste0("Figures/Country/10_linear_investment/RDS/linear_investment_mean.rds"))

theme_txt <- theme(title = element_text(size = 11,
                                        face = 'bold'),
                   axis.title = element_text(size = 10#,
                                             #face = 'bold')
                   ))

layout <- c(
  patchwork::area(t = 1, l = 1, b = 50, r = 60),
  patchwork::area(t = 55, l = 1, b = 105, r = 60),
  patchwork::area(t = 110, l = 1, b = 190, r = 60)
)

figure_02 <- area_boxplot + resilience_boxplot + linear +
  plot_layout(design = layout) &
  theme_txt

dir.create("Figures/Country/04b_patchwork_boxplot_investment/", recursive = TRUE)

ggsave(plot = figure_02, paste0("Figures/Country/04b_patchwork_boxplot_investment/patchwork_boxplot_investment_mean_", prct, ".pdf"),
       dpi = 300, width = 18, height = 27, units = "cm")

ggsave(plot = figure_02, paste0("Figures/Country/04b_patchwork_boxplot_investment/patchwork_boxplot_investment_mean_", prct, ".jpg"),
       dpi = 300, width = 18, height = 27, units = "cm")

# plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
# .rs.restartR()