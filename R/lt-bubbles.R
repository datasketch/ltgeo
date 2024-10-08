#' @export
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
  theme_opts <- dsopts_merge(..., categories = "theme")
  bubble_min <- opts_bubble$bubble_min %||% 3
  bubble_max <- opts_bubble$bubble_min %||% 15
  data_viz <- data_prep_bubbles(data = data,
                                dic = dic,
                                var_cat = var_cat, var_geo = var_geo,
                                var_gln = var_gln, var_glt = var_glt,
                                var_num = var_num,
                                map_name = opts_data$map_name, opts = opts_data)
  opts_colors <- dsopts_merge(..., categories = "colorprep")
  if (!"..colors" %in% names(data_viz)) {
    color_type <- opts_colors$color_palette_type %||% "sequential"
    if (!is.null(var_cat)) color_type <- "categorical"
    color_palette <- opts_colors[[paste0("color_palette_", color_type)]]
  } else {
    color_palette <- data_viz$..colors
  }

  if (color_type == "categorical") {
    pal <-  leaflet::colorFactor(
      palette = color_palette,
      domain = data_viz$value
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette = color_palette,
      domain = data_viz$value,
      na.color = opts_colors$na_color
    )
  }
  add_cluster <- NULL
  if (opts_bubble$map_bubble_cluster) {
    add_cluster <- do.call(eval(parse(text="leaflet::markerClusterOptions")),
                           opts_bubble$map_bubble_cluster_params %||% list(showCoverageOnHover = FALSE))
  }

  lt <- base_map |>
    addCircleMarkers(
      data = data_viz,
      lng = ~lon,
      lat = ~lat,
      radius = ~scales::rescale(..domain, c(bubble_min, bubble_max)),
      # layerId = opts$layer_id,
      # group = opts$group,
      color = ~pal(value),
      weight = general_opts$border_width,
      opacity = general_opts$bubble_opacity,
      fillColor = ~pal(value),
      fillOpacity =  general_opts$map_opacity %||% 0.8,
      # dashArray =  opts$map_circle_dashArray,
      # popup = opts$popup,
      # popupOptions = opts$popup_options,
      label = ~..labels,
      labelOptions = labelOptions(
        opacity = 1,
        style = list("font-weight" = "normal", padding = "8px 10px"),
        textsize = paste0(theme_opts$tooltip_text_size, "px"),
        direction = "auto"
      ),
      clusterOptions = add_cluster#,
      # clusterId = opts$map_cluster_id
    )
  lt
}
