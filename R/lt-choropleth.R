#' @export
lt_choropleth <- function(data = NULL,
                          dic = NULL,
                          var_geo = NULL,
                          var_cat = NULL,
                          var_num = NULL,
                          ...) {
  data_viz <- NULL
  dic_viz <- NULL

  if (!is.null(data)) {
    ht <- hdtable(data, dic)
    dic_viz <- ht$dic
    data_viz <- ht$data
  }

  opts_data <- dsopts_merge(..., categories = "dataprep")

  data_viz <- data_prep(
    data_viz, dic_viz,
    var_geo, var_cat, var_num,
    map_name = opts_data$map_name,
    map_file = opts_data$map_file,
    opts = opts_data
  )

  opts_colors <- c(
    dsopts_merge(..., categories = "legend"),
    dsopts_merge(..., categories = "colorprep")
  )

  if (!"..colors" %in% names(data_viz)) {
    color_type <- opts_colors$color_palette_type
    if (is.null(color_type)) color_type <- "sequential"

    color_palette <- opts_colors[[paste0("color_palette_", color_type)]]
    if (is.null(data)) color_palette <- "transparent"

    color_func <- dplyr::case_when(
      is.null(var_cat) & color_type == "categorical" ~ "colorBin",
      !is.null(var_cat) & color_type == "categorical" ~ "colorFactor",
      .default = "colorNumeric"
    )
  } else {
    color_palette <- data_viz$..colors
  }

  color_func_opts <- list(
    palette = color_palette,
    domain = data_viz$value,
    na.color = opts_colors$na_color
  )

  if (color_func == "colorBin") {
    color_func_opts$bins <- opts_colors$color_bins_n
  }

  pal <- do.call(color_func, color_func_opts)

  zoom_opts <- dsopts_merge(..., categories = "zoom")
  general_opts <- dsopts_merge(..., categories = "map")
  tiles_opts <- dsopts_merge(..., categories = "maptiles")
  highlight_opts <- dsopts_merge(..., categories = "highlight")
  theme_opts <- dsopts_merge(..., categories = "theme")
  title_opts <- c(
    dsopts_merge(..., categories = "text"),
    dsopts_merge(..., categories = "titles")
  )

  lt <- data_viz |>
    leaflet(
      options = leafletOptions(
        attributionControl = general_opts$attribution_control,
        zoomControl = FALSE,
        zoomSnap = zoom_opts$map_zoom_snap,
        zoomDelta = zoom_opts$map_zoom_delta,
        zoom = zoom_opts$zoom_level,
        minZoom = zoom_opts$zoom_min,
        maxZoom = zoom_opts$zoom_max
      )
    ) |>
    lt_tiles(tiles_opts) |>
    addPolygons(
      fillColor = ~ pal(value),
      weight = general_opts$border_width,
      opacity = general_opts$border_opacity,
      color = general_opts$border_color %||% "#FFFFFF",
      fillOpacity = general_opts$map_opacity %||% 0.8,
      highlight = highlightOptions(
        weight = highlight_opts$highlight_border_weight,
        color = highlight_opts$highlight_border_color,
        fillOpacity = highlight_opts$highlight_opacity,
        bringToFront = general_opts$map_bring_front
      ),
      label = ~..labels,
      labelOptions = labelOptions(
        opacity = 1,
        style = list("font-weight" = "normal", padding = "8px 10px"),
        textsize = paste0(theme_opts$tooltip_text_size, "px"),
        direction = "auto"
      )
    )

  lt <- lt |>
    lt_add_layers(general_opts$map_name_layers)

  lt <- lt |>
    lt_titles(title_opts)

  if (zoom_opts$zoom_show) {
    lt <- lt |>
      lt_add_zoom_control(zoom_opts$map_zoom_control_align %||% "topright")
  }

  if (!is.null(data)) {
    lt <- lt |> leaflet::addLegend(
      pal = pal,
      values = data_viz$value,
      title = title_opts$title_legend,
      position = "bottomright"
    )
  }

  lt
}

#' @export
lt_choropleth_Geo <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  lt_choropleth(data, dic, var_geo = vars[1], ...)
}

#' @export
lt_choropleth_GeoNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  lt_choropleth(data, dic, var_geo = vars[1], var_num = vars[2], ...)
}

#' @export
lt_choropleth_GeoCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  lt_choropleth(
    data, dic, var_geo = vars[1], var_cat = vars[2],
    color_palette_type = "categorical",
    ...
  )
}

#' @export
lt_choropleth_Gnm <- lt_choropleth_Geo
#' @export
lt_choropleth_GnmNum <- lt_choropleth_GeoNum
#' @export
lt_choropleth_GnmCat <- lt_choropleth_GeoCat

#' @export
lt_choropleth_Gcd <- lt_choropleth_Geo
#' @export
lt_choropleth_GcdNum <- lt_choropleth_GeoNum
#' @export
lt_choropleth_GcdCat <- lt_choropleth_GeoCat
