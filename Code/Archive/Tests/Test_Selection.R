#Packages
pacman::p_load(terra, prioritizr, tidyverse, patchwork)

#set target
target_prior <- 0.5

#Produce a random raster (cost = area of mangroves)
raster_cost <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
values_cost <- runif(ncell(raster_cost), min = 0, max = 1) %>% 
  sort()
  
raster_cost <- setValues(raster_cost, values_cost) 

#Produce a raster with the features (presence-absence)
raster_feature <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)

raster_feature_list <- lapply(1:5, function(i) {
  values_raster <- runif(ncell(raster_feature), min = 0, max = 1) %>% 
    round(digits = 0)
  setValues(raster_feature, values_raster)
  }
  )

raster_feature <- rast(raster_feature_list)

#Transform presence-absence to area cover based on raster_cost
raster_feature <- ifel(raster_feature > 0, raster_cost, 0) %>% 
  raster::stack()

#Transform to raster that can be read by prioritizr
raster_cost <- raster_cost %>% 
  raster::stack()

names(raster_feature) <- c("f1", "f2", "f3", "f4", "f5")

#Prioritizr problem with the area of mangroves as a cost
prior_problem_areacost <- problem(x = raster_cost, features = raster_feature) %>%
  add_relative_targets(0.3) %>% 
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = TRUE, gap = 0)

solution_areacost <- solve(prior_problem_areacost)

plot(solution_areacost)
plot(raster_cost)

#Plot the results
cost_df <- raster_cost %>% 
  as.data.frame(xy = TRUE)

solution_df <- solution_areacost %>% 
  as.data.frame(xy = TRUE) %>% 
  add_column(cost = cost_df[, 3]) %>% 
  mutate(solution = as.factor(lyr.1)) %>% 
  dplyr::select(!lyr.1) %>% 
  arrange(cost)

plot_solution_areacost <- ggplot(data = solution_df, aes(x = x, y = y, colour = solution, fill = cost)) +
  geom_tile(aes(width = 0.9, height = 0.9), linewidth = 1) +
  scale_colour_manual(values = c("0" = "white", "1" = "red")) +
  theme_void()

eval_cost_summary(prior_problem_areacost, solution_areacost)

raster_feature_df <- raster_feature %>% 
  as.data.frame(xy = TRUE)

plot_features <- lapply(c(1:5), function(index_feature) {
  ggplot(data = raster_feature_df, aes(x = x, y = y, fill = !!sym(names(raster_feature)[index_feature]))) +
    geom_tile(aes(width = 0.9, height = 0.9), linewidth = 1) +
    scale_fill_viridis_c() +
    theme_void()
})

richness <- raster_feature_df %>% 
  rowwise() %>% 
  mutate(tot_feature_area = sum(c(f1, f2, f3, f4, f5)))

plot_richness <- ggplot(data = richness, aes(x = x, y = y, fill = tot_feature_area)) +
  geom_tile(aes(width = 0.9, height = 0.9), linewidth = 1) +
  scale_fill_viridis_c(option = "A") +
  theme_void()

################################################################################
#produce another raster with using a cost = 1 for each planning unit
raster_cost <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
raster_cost <- setValues(raster_cost, 1) %>% 
  raster::raster()

#Produce a problem using the homogeneous cost
prior_problem_homcost <- problem(x = raster_cost, features = raster_feature) %>%
  add_relative_targets(target_prior) %>% 
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = TRUE, gap = 0)

solution_homcost <- solve(prior_problem_homcost)

plot(solution_homcost)

solution_homcost_df <- solution_homcost %>% 
  as.data.frame(xy = TRUE)
cost_df <- raster_cost %>% 
  as.data.frame(xy = TRUE)

solution_homcost_df <- solution_homcost_df %>% 
  add_column(cost = cost_df[, 3]) %>% 
  mutate(solution = as.factor(lyr.1))

plot_solution_homcost <- ggplot(data = solution_homcost_df, aes(x = x, y = y, colour = solution, fill = cost)) +
  geom_tile(aes(width = 0.9, height = 0.9), linewidth = 1) +
  scale_colour_manual(values = c("0" = "white", "1" = "red")) +
  theme_void()

final_plot <- plot_solution_areacost + plot_solution_homcost + plot_richness + 
  plot_spacer() + plot_spacer() + plot_features + plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'A')

ggsave(paste0("Results/Figures/Tests/Test_AreaBased_", 
              target_prior, ".png"), width = 20, height = 10, dpi = 300)

#Kappa
source("Code/Functions/f_create_KappaCorrPlot.R")

Kappa <- fcreate_kappacorrplot(c(rast(solution_areacost), rast(solution_homcost)), 
                      name_sol = c("area cost", "homogeneous cost"))

Kappa[[2]]
