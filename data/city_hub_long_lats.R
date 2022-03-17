# city center lon/lat queen st x albert st
bris_cc <- st_point(c(153.025241,-27.469652), dim = "XY") # Brisbane city
ip_cc <- st_point(c( 152.7600,-27.6230), dim = "XY") # Ipswich
gc_cc <-  st_point(c(153.4000,-28.0167), dim = "XY") # Gold Coast
ma_cc <- st_point(c(153.1000,-26.6500), dim = "XY") # Maroochydore


hub_dist <- function(geometry){
  
  min(c(
    st_distance(geometry, bris_cc),
    #st_distance(geometry, ip_cc),
    st_distance(geometry, gc_cc),
    st_distance(geometry, ma_cc)
  ))
  
} 

min_distance_to_hub <- Vectorize(hub_dist)