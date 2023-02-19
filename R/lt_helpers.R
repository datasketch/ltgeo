





#' Graticule map
lt_graticule <- function(map, graticule) {

  if (graticule$map_graticule) {
    map <- map %>%
      leaflet::addGraticule(interval = graticule$map_graticule_interval,
                            style = list(color = graticule$map_graticule_color, weight = graticule$map_graticule_weight))
  }
  map
}





#' titles map




