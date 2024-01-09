#' @keywords internal
ltg_choropleth <- function(map, opts) {

  lf <-  map |>
    leaflet::addPolygons(
      layerId = ~name,
      group = opts$group,
      stroke = opts$stroke,
      color = opts$border_color,
      weight = opts$weight,
      opacity = opts$opacity,
      fill = opts$fill,
      fillColor = opts$map_color %||% ~opts$pal(..domain),
      fillOpacity = opts$fill_opacity,
      dashArray = opts$dashArray,
      smoothFactor = opts$smooth_factor,
      noClip = opts$no_clip,
      popup = opts$popup,
      popupOptions = opts$popup_options,
      label = ~label,
      labelOptions = opts$label_options,
      highlightOptions = opts$highlight_options
    )


  if (opts$map_extra_layer) {
    topoData <- geojsonio::topojson_json(geodato::gd_tj(opts$map_name_extra))
    lf <- lf |>
      leaflet::addTopoJSON(
        topojson = topoData,
        weight = opts$map_extra_weight,
        opacity = opts$map_extra_opacity,
        fill = F,
        fillColor = opts$map_extra_fillColor,
        color = opts$map_extra_color)
  }
  lf

}

#' @keywords internal
ltg_circles <- function(map, opts) {

  cluster_add <- NULL
  if (opts$cluster_add) {
    cluster_add <- do.call(eval(parse(text="leaflet::markerClusterOptions")), opts$cluster_opts)
  }

  if (opts$map_basic) {
    map <- map |>
      ltg_choropleth(opts$basic_choropleth)
  }
  tryCatch({
    map |>
      leaflet::addCircleMarkers(
        data = opts$extra_data %||% getMapData(map),
        lng = ~lon,
        lat = ~lat,
        radius = opts$map_radius %||%
          ~scales::rescale(..domain, c(opts$map_min, opts$map_max)),
        layerId = opts$layer_id,
        group = opts$group,
        stroke = opts$stroke,
        color = opts$map_circle_color,
        weight = opts$map_circle_weight,
        opacity = opts$map_circle_opacity,
        fill = opts$fill,
        fillColor = opts$map_circle_color %||% ~opts$pal(..domain),
        fillOpacity =  opts$map_circle_fill_opactity,
        dashArray =  opts$map_circle_dashArray,
        popup = opts$popup,
        popupOptions = opts$popup_options,
        label = ~label,
        labelOptions = opts$label_options,
        clusterOptions = cluster_add,
        clusterId = opts$map_cluster_id
      )}, error = function(e) {
        map
      })
}

#' @keywords internal
ltg_hexmap <- function(map, opts) {

  if (opts$map_basic) {
    map <- map |>
      ltg_choropleth(opts$basic_choropleth)
  }

  tryCatch({
    leaflet.extras2::addHexbin(
      data = opts$extra_data %||% getMapData(map),
      map = map,
      lng = ~lon,
      lat = ~lat,
      radius = opts$map_radius,
      layerId = opts$layer_id,
      group = opts$group,
      opacity = opts$map_hex_opacity,
      options = leaflet.extras2::hexbinOptions(duration = 200,
                                               colorScaleExtent = NULL,
                                               radiusScaleExtent = NULL,
                                               colorRange = c(opts$colors[1], opts$colors[2]),
                                               radiusRange = c(opts$map_min, opts$map_max),
                                               pointerEvents = "all",
                                               resizetoCount = FALSE,
                                               tooltip = FALSE)
    )
  }, error = function(e) {
    map
  })
}

lt_titles <- function(map, titles) {

  map %>%
    leaflet::addControl(titles$caption,
                        position = "bottomleft",
                        className="map-caption") %>%
    leaflet::addControl(titles$title,
                        position = "topleft",
                        className="map-title") %>%
    leaflet::addControl(titles$subtitle,
                        position = "topleft",
                        className="map-subtitle")

}
