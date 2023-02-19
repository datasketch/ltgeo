
#' Background and branding Map
lt_background <- function(map, opts) {
  theme <- opts$theme
  #print(theme$map_provider_tile)
  if (is.null(theme$map_tiles) & theme$map_provider_tile == "leaflet") {
    lf <- map %>% leaflet.extras::setMapWidgetStyle(list(background = theme$background_color))
  } else {
    if (theme$map_provider_tile == "leaflet") {
      lf <- map %>% leaflet::addProviderTiles(theme$map_tiles)
    } else {
      lf <- map %>% leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers[[theme$map_tiles_esri]])
      if (!is.null(theme$map_extra_layout)) {
        lf <- lf %>%
          leaflet.esri::addEsriFeatureLayer(
            url = theme$map_extra_layout,
            labelProperty = theme$map_name_layout)
      }
    }
  }
  if (theme$branding_include) {
    img <- url_logo(logo = theme$logo, background = theme$background_color)
    lf <- lf %>%
      leafem::addLogo(img, width = theme$logo_width, height = theme$logo_height, position = "bottomright")
  }
  lf
}
