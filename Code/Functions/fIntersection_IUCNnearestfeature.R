#Author: Alvise Dabalà
#Function to intersect IUCN species distribution with the planning units

#Inputs: 
# - PUs: planning units shapefile
# - IUCN: species distribution shapefile

fIntersection_IUCNnearestfeature <- function(PUs, IUCN) {
 
  #I project the PUs
  PUs <- PUs %>%
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  #Intersection and selection of the variables of interest
  PUs <- st_join(PUs, IUCN_mangroves, join = st_intersects) %>%
    dplyr::select(c(colnames(PUs), "sci_name"))
    
  PUs$area_m2 <- as.numeric(PUs$area_m2) 
  
  PUs$area_m2_01 <- PUs$area_m2 #Create a new column that will be lost later
  
  PUs <- PUs %>% 
    as_tibble() %>% #Transform in a tibble
    group_by(sci_name) %>% 
    pivot_wider(names_from = sci_name, values_from = area_m2_01, 
                values_fn = sum, names_prefix = "Bin_", values_fill = 0) %>% #Use value fn because some names are considered duplicated but different
    st_sf() #Transform in a sf
    
  #Production of PUs without species information
  PUsNoInfo <- PUs %>%
    dplyr::filter(`Bin_NA` != 0) #Select all the PUs that do not intersect with IUCN - Redlist (Bin_NA = area that do not intersect)
  
  #Production of PUs with species information
  PUsInfo <- PUs %>%
    dplyr::filter(`Bin_NA` == 0) #Select all the PUs that intersect with IUCN - Redlist
  
  ClosestPUs <- st_nearest_feature(PUsNoInfo, PUsInfo) #Produce a list with the number of row of the nearest PUsInfo to PUsNoInfo
  
  # c <- PUsNoInfo %>% #Select row i of PUsNoInfo
  #   as_tibble() %>% #Transform to tibble
  #   dplyr::select(`Bin_NA`) #Select the variable Bin_NA of those columns
      
  AreaValues <- PUsInfo[ClosestPUs,] %>% #Select the row a of PUsInfo
    as_tibble() %>% #Transform in tibble
    dplyr::select(starts_with("Bin_")) %>% #Select all the column that start with Bin_
    mutate_at(vars(everything()),
                list(~ ifelse( . == 0, 0, PUsNoInfo$`Bin_NA`))) #I mutate all the columns (that start with Bin_) to the value of Bin_NA if they are not equal to zero, if not to zero
    
  PUsNoInfo <- PUsNoInfo %>% #Select row i of PUsNoInfo
    as_tibble() %>% #Transform to tibble
    dplyr::select(-starts_with("Bin_")) %>% #Remove all the variable that start with Bin_
    add_column(AreaValues) %>% #Add all the column of d
    st_sf() #Produce a shapefile
  
  PUs <- rbind(PUsInfo, PUsNoInfo) %>% #I bind PUsInfo and PUsNoInfo
    dplyr::select(-"Bin_NA") #I remove the column Bin_NA
  
  #Select only the columns of mangroves species
  PUs_sp <- PUs %>% 
    dplyr::select(starts_with("Bin_")) %>%
    st_drop_geometry() %>%
    rename_all(~stringr::str_replace(.,"^Bin_",""))
  
  #Calculate the number of PUs that intercept with each species distribution
  nPUs <- (colSums(PUs_sp != 0)) %>%
    bind_rows() %>%
    pivot_longer(everything()) %>%
    rename(binomial = name)
  
  #Number of species in each PU
  PUs_Species <- PUs_sp %>% 
    mutate(tot = rowSums(PUs_sp != 0)) %>% 
    dplyr::select(tot) %>% 
    mutate(geometry = PUs$geometry) %>% 
    st_sf()
  
  #Remove the Bin_ before the name of the species and arrange by species
  PUs <- PUs %>%  
    rename_all(~stringr::str_replace(.,"^Bin_","")) %>% 
    arrange(ID)
  
  #List that will be returned by the function
  Result <- list(PUs, nPUs, PUs_Species)
  
  return(Result)
}