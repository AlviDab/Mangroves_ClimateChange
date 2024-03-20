# 20-03-2024
## Function to substitute NA values in the columns with those from the nearest PU

#Input:
# - x <sf>: planning units
# - colName <string>: name of the column that report the number of intersections

fNN_NAs <- function(x, colName) {
  x <- x %>%
    arrange(ID)

  Value <-  x %>%
    filter(!is.na(.data[[colName]]))

  NoValue <- x %>%
    filter(is.na(.data[[colName]]))

  NN_Value <- Value %>%
    slice(as_vector(st_nearest_feature(NoValue, Value))) %>%
    st_drop_geometry() %>%
    dplyr::select(all_of(colName)) %>%
    as_vector()

  NoValue <- NoValue %>%
    mutate(!!colName := NN_Value) #Check this

  x <- bind_rows(Value, NoValue) %>%
    arrange(ID)

  return(x)
}