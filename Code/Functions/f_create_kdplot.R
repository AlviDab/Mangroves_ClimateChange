# Author: Tin Buenafe & Alvise Dabalà
# Last updated: 05-04-22

# Purpose: To create kernel density plots of any df.
# Input:
# 1. df: data frame with the following column names
# - rows (could be features, planning units, etc.)
# - other column names represent the different groups (e.g. different solutions)
# the values represent the values that we want frequency distributions of (e.g. % of features, ecosystem benefit)
# - values under the rows column indicate the feature name or planning unit number
# 3. x_lab: the text for the x label
# 4. y_lab: the text for the y label
# 5. title_leg: the text for the title of the legend
# 6. logarithmic: equals TRUE if you want to transform the data to log scale
# 7. central_tendency: you can decide if plot the mean or the median of the data having
# "mean" of "median" in input ("mean" is the default option)

# Output:
# ggplot of kernel density plot which you can manipulate just as you would a ggplot object
# e.g. use of this code would be:
# fcreate_kdplot(df, palette) +
# geom_vline(xintercept = c(30), linetype = "dashed", color = "red", size = 1)
################################################################################

f_create_kdplot <- function(df, x_lab = NULL, y_lab = NULL, title_leg = "Legend",
                            logarithmic = FALSE, central_tendency = "mean") {

  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ggridges)

  if(logarithmic == TRUE) {
    df <- df %>%
      mutate(across(!rows, log))
  }

  x <- df %>%
    tidyr::pivot_longer(!rows, names_to = "group", values_to = "value") %>%
    dplyr::mutate(row_number = row_number(rows)) %>%
    filter(value != 0) #Remove all the zero values (no selection)

  if(central_tendency == "mean") {
    tendency_df <- x %>%
      group_by(group) %>%
      summarise(value = mean(value))
  }

  if(central_tendency == "median") {
    tendency_df <- x %>%
      group_by(group) %>%
      summarise(value = mean(value))
  }

  quantile(x$value, 0.01)

  ggRidge <- ggplot(data = x) +
    geom_violin(aes(x = value, y = group, group = group, fill = group),
                scale = 2, size = 0.1) +
    stat_summary(fun.data = "mean_sdl", aes(x = value, y = group), fun.args = list(mult = 1),
                 geom = "pointrange", color = "grey", size = 0.1
    ) +
    scale_fill_viridis_d(name = title_leg, option = "A") +
    xlim(quantile(x$value, 0.01), max(x$value)) +
    # geom_vline(data = tendency_df , aes(xintercept = value,
    #                                     colour = group),
    #            #linetype = "dashed",
    #            size = 1, show.legend = FALSE) +
    # scale_colour_viridis_d(name = title_leg, option = "B") +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_y_discrete(expand = c(0, 0), position = "right") +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 7))

  return(ggRidge)
}
