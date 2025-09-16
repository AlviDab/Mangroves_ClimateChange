#Benchmark parallel sf
pacman::p_load(tictoc, furrr, parallel, future, bench)

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  st_break_antimeridian(lon_0 = 0) %>%
  st_make_valid()

world <- world[130:149,]

world_polygon <- world %>%
  st_cast("POLYGON")

world %>% st_area() %>% sum()

world_polygon %>% st_area() %>% sum()

#Parallel computation time
n_cores <- parallel::detectCores() - 2

fold_length <- floor(nrow(world_polygon) / n_cores)

split_vector <- rep(x = 1:n_cores,
                    times = c(rep(x = fold_length, times = n_cores - 1),
                              nrow(world_polygon) - fold_length * (n_cores - 1)))

bench::mark(
  'st_cast'= {

    st_intersection(world_polygon, PUs)
  },

  'st_cast_parallel'= {
    plan(multisession, workers = n_cores)

    split_results <- split(world_polygon, split_vector) %>%
      furrr::future_map(function(x) st_intersection(x, PUs))
  },

  check = FALSE
)