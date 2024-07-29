#' @keywords internal
lt_tiles <- function(lt, opts) {
  if (is.null(opts$map_provider_tile)) {
    lt <- lt |>
      leaflet.extras::setMapWidgetStyle(list(background = opts$background_color))
  } else {
    switch(opts$map_provider_tile,
           "leaflet" = lt <- lt |> leaflet::addProviderTiles(opts$map_tiles),
           "esri" = {
             lt <- lt |> leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers[[opts$map_tiles_esri]])
             if (!is.null(opts$map_extra_layout)) {
               lt <- lt |>
                 leaflet.esri::addEsriFeatureLayer(
                   url = opts$map_extra_layout)
             }
           },
           {
             lt <- lt |> leaflet::addTiles(urlTemplate = opts$map_extra_layout)
           }
    )
  }

  lt
}

