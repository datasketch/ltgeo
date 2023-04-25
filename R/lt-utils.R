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
