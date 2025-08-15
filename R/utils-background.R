#' @keywords internal
lt_tiles <- function(lt, opts) {
  # Si no se especifica proveedor de mapas, solo se aplica el color de fondo
  if (is.null(opts$map_provider_tile)) {
    return(lt |> leaflet.extras::setMapWidgetStyle(list(background = opts$background_color)))
  }

  # Si el proveedor es "leaflet"
  if (opts$map_provider_tile == "leaflet") {
    if (is.null(opts$map_tiles) || opts$map_tiles == "") {
      lt <- lt |> leaflet.extras::setMapWidgetStyle(list(background = opts$background_color))
    } else {
      lt <- lt |> leaflet::addProviderTiles(opts$map_tiles)
    }

  # Si el proveedor es "esri"
  } else if (opts$map_provider_tile == "esri") {
    esri_layer <- leaflet.esri::esriBasemapLayers[[opts$map_tiles_esri]]
    lt <- lt |> leaflet.esri::addEsriBasemapLayer(esri_layer)

    if (!is.null(opts$map_extra_layout)) {
      lt <- lt |> leaflet.esri::addEsriFeatureLayer(url = opts$map_extra_layout)
    }

  # Otro proveedor (personalizado)
  } else {
    lt <- lt |> leaflet::addTiles(urlTemplate = opts$map_extra_layout)
  }

  return(lt)
}
