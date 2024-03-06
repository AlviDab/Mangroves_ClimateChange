#Create PUs

EEZ_land <- st_read("Data/ne_10m_coastline/ne_10m_coastline.shp") %>% 
  st_crop(xmin = 100, ymin = -45, xmax = 105, ymax = 35) %>% 
  st_transform(cCRS)

sf_use_s2(FALSE)

buff <- EEZ_land %>% 
  st_buffer(30000) %>% 
  st_union() %>% 
  st_cast()

buff_crop <- buff %>% 
  st_make_valid()

PUs <- map(buff_crop, function(x) {
  
  PUs_small <- x %>% 
    st_cast("MULTILINESTRING") %>% 
    st_make_grid(cellsize = 100000, square = FALSE, what = "polygons") %>% 
    st_as_sf(crs = cCRS)
  
  PUs_small_int <- PUs_small %>% 
    st_intersects(buff_crop) %>% 
    lengths > 0
  
  PUs_small[PUs_small_int,]
  
})

PUs_unlisted <- PUs %>% 
  bind_rows()

mapview::mapview(PUs)
  
PUs_int <- PUs %>% 
  st_intersects(buff_crop)

PUs_filtered <- PUs[PUs_int %>% lengths > 0] %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_as_sfc()

tmap::qtm(PUs_filtered)

PUs_final <- map(PUs_filtered, function(x) {
  PUs_small <- x %>% 
    st_make_grid(cellsize = 100000, square = FALSE, what = "polygons") %>% 
    st_as_sf(crs = cCRS)
  
  PUs_small_int <- PUs_small %>% 
    st_intersects(PUs_filtered) %>% 
    lengths > 0
  
  PUs_small[PUs_small_int,]
})

PUs_final_final <- PUs_mangroves %>% 
  st_intersect

saveRDS(PUs_final, "buff.rds")
st_write(PUs_unlisted, "buff.gpkg", append = FALSE)
