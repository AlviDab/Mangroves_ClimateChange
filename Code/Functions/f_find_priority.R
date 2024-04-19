#Author: Alvise Dabalà
#Date: 23/02/2024

f_find_priority <- function(PUs, col_name, prct_priority) {

  #Find names of all features
  names_features <- PUs %>%
    dplyr::select(starts_with("Sp_")) %>%
    st_drop_geometry() %>%
    names()

  #Function to find the priority areas for each feature
  find_priority <- function(feature_name) {
    PUs_CC <- PUs %>%
      dplyr::select(ID,
                    all_of(col_name),
                    all_of(feature_name)) %>%
      st_drop_geometry() %>%
      filter(.data[[feature_name]] > 0) %>%
      arrange(-.data[[col_name]], .data[[feature_name]]) %>%
      mutate(cumulative_area = cumsum(.data[[feature_name]]))

    Tot_area_feature <- PUs_CC %>%
      summarise(sum(.data[[feature_name]])) %>%
      as.numeric()

    PUs_CC <- PUs_CC %>%
      mutate(priority = case_when(cumulative_area < Tot_area_feature*prct_priority ~ TRUE,
                                  .default = FALSE)) %>%
      mutate(areas_to_avoid = case_when(cumulative_area > Tot_area_feature*(1-prct_priority) ~ TRUE,
                                        .default = FALSE)) %>%
      dplyr::select(ID,
                    priority,
                    areas_to_avoid)

    PUs_priority <- PUs %>%
      dplyr::select(ID) %>%
      left_join(PUs_CC, by = "ID") %>%
      arrange(ID) %>%
      dplyr::select(priority) %>%
      st_drop_geometry()
  }

  ncores <- detectCores() - 2

  plan(multisession, workers = ncores)

  priority <- future_map(names_features, find_priority,
                         .options = furrr_options(seed = TRUE))

  plan(sequential)

  priority <- priority %>%
    bind_cols() %>%
    reframe(priority = rowSums(., na.rm = TRUE)) %>%
    mutate(priority = case_when(priority > 0 ~ TRUE,
                                .default = FALSE))

  PUs <- PUs %>%
    add_column(priority)

}
