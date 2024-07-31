lt_bubbles <- function(data = NULL,
                       dic = NULL,
                       var_gln = NULL,
                       var_glt = NULL,
                       var_geo = NULL,
                       var_num = NULL,
                       var_cat = NULL,
                       ...) {

  base_map <- lt_choropleth(...)
  if (is.null(data)) return(base_map)
  opts_data <- dsopts_merge(..., categories = "dataprep")
  general_opts <- dsopts_merge(..., categories = "map")
  opts_bubble <- dsopts_merge(..., categories = "mapbubble")
  bubble_min <- opts_bubble$bubble_min %||% 3
  bubble_max <- opts_bubble$bubble_min %||% 15
  data_viz <- data_prep_bubbles(data = data,
                                dic = dic,
                                var_cat = var_cat, var_geo = var_geo,
                                var_gln = var_gln, var_glt = var_glt,
                                var_num = var_num,
                                map_name = opts_data$map_name, opts = opts_data)
  add_cluster <- NULL
  if (opts_bubble$map_bubble_cluster) {
    add_cluster <- do.call(eval(parse(text="leaflet::markerClusterOptions")),
                           opts_bubble$map_bubble_cluster_params %||% list(showCoverageOnHover = FALSE))
  }

  lt <- base_map |>
    leaflet::addCircleMarkers(
      data = data_viz,
      lng = ~lon,
      lat = ~lat,
      radius = ~scales::rescale(value, c(bubble_min, bubble_max)),
      # layerId = opts$layer_id,
      # group = opts$group,
      # stroke = opts$stroke,
      # color = opts$map_circle_color,
      weight = general_opts$border_width,
      opacity = general_opts$bubble_opacity,
      # fill = opts$fill,
      # fillColor = opts$map_circle_color %||% ~opts$pal(..domain),
      fillOpacity =  general_opts$map_opacity %||% 0.8,
      # dashArray =  opts$map_circle_dashArray,
      # popup = opts$popup,
      # popupOptions = opts$popup_options,
      label = ~..labels,
      # labelOptions = opts$label_options,
      clusterOptions = add_cluster#,
      # clusterId = opts$map_cluster_id
    )
  lt
}
