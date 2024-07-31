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
  opts_bubble <- dsopts_merge(..., categories = "mapbubble")
  bubble_min <- opts_bubble$bubble_min %||% 3
  bubble_max <- opts_bubble$bubble_min %||% 15
  data_viz <- data_prep_bubbles(data = data,
                                dic = dic,
                                var_cat = var_cat, var_geo = var_geo,
                                var_gln = var_gln, var_glt = var_glt,
                                var_num = var_num,
                                map_name = opts_data$map_name, opts = opts_data)
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
      # weight = opts$map_circle_weight,
      #opacity = opts_bubble$bubble_opacity,
      # fill = opts$fill,
      # fillColor = opts$map_circle_color %||% ~opts$pal(..domain),
      #fillOpacity =  opts_bubble$bubble_opacity,
      # dashArray =  opts$map_circle_dashArray,
      # popup = opts$popup,
      # popupOptions = opts$popup_options,
      label = ~..labels#,
      # labelOptions = opts$label_options,
      # clusterOptions = cluster_add,
      # clusterId = opts$map_cluster_id
    )
  lt
}
