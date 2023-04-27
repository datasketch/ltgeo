#' @keywords internal
ltg_choropleth <- function(map, opts) {

  map |>
    addPolygons(
      layerId = opts$layer_id,
      group = opts$group,
      stroke = opts$stroke,
      color = opts$color,
      weight = opts$weight,
      opacity = opts$opacity,
      fill = opts$fill,
      fillColor = ~opts$pal(..domain),
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
}

#' @keywords internal
ltg_circles <- function(map, opts) {

  cluster_add <- NULL
  if (opts$cluster_add) {
    cluster_add <- do.call("markerClusterOptions", opts$cluster_opts)
  }

  map |>
    addCircleMarkers(
      data = opts$extra_data %||% getMapData(map),
      lng = ~lon,
      lat = ~lat,
      radius = opts$map_radius %||% ~scales::rescale(..domain, 3, 15),
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
      popup = opts$map_circle_pupup,
      popupOptions = opts$map_circle_popupOptions,
      label = ~label,
      labelOptions = opts$map_circle_labelOptions,
      clusterOptions = cluster_add,
      clusterId = opts$map_cluster_id
    )


}

