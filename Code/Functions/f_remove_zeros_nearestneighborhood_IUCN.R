# 19-02-2024
## Function to substitute all species values in a PU that does not intersect
## with IUCN species distribution with those from the nearest PU

#Input:
# - x <sf>: planning units
# - colName <string>: name of the column that report the number of intersections

fNN_zeros_IUCN <- function(x, colName) {
  x <- x %>%
    arrange(ID)

  Value <-  x %>%
    filter(.data[[colName]] > 0)

  NoValue <- x %>%
    filter(.data[[colName]] == 0)

  NN_Value <- Value %>%
    slice(as_vector(st_nearest_feature(NoValue,
                                       Value))) %>%
    mutate(ID = NoValue$ID)

  x <- bind_rows(Value, NN_Value) %>%
    arrange(ID)

  return(x)
}
