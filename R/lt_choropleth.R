

#' Leaflet choropleths
#'
#' @name lt_choropleth
#' @param data A data.frame
#' @return ggplot viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' gg_choropleth_GcdNum(sample_data("Gcd-Num", nrow = 10))
lt_choropleth <- function(data = NULL, map_name = NULL, var = NULL,
                          filter = NULL, ...){


  if(is.null(map_name))
    stop("No map name provided, see available_maps()")

  col <- geodato::parse_col(data, var)

  opts <- dsvizopts::merge_dsviz_options(...)
  #opts <- dsvizopts::merge_dsviz_options()

  data$..var <- data[[col]]




  d <- geodato::gd_match(data, map_name)
  tj <- geodato::gd_tj(map_name)

  if(!is.null(data)){
    dgeo <- tj |>
      dplyr::left_join(d, by = c(id = "..gd_id", name = "..gd_name"))
  } else{
    dgeo <- d
  }

  # Calculate color variable

  if(is.numeric(dgeo$..var)){
    palette <- opts$theme$palette_colors_sequential
  }else{
    palette <- opts$theme$palette_colors_categorical
  }
  dgeo$..color <- paletero::paletero(dgeo$..var,
                                               palette = palette,
                                               na.color = opts$theme$na_color)

  # Calculate tooltip
  vars <- names(data)[!grepl("^\\.\\.|geometry", names(data))]
  tooltip <- opts$chart$tooltip %||% NULL
  dd <- sf::st_drop_geometry(dgeo) |>
    select(name, any_of(vars))
  dgeo$..labels <- dsdataprep::prep_tooltip(data = dd, tooltip = tooltip,
                                   na_row_default_column = "name")

  # Filter regions
  if(!is.null(filter)){
    code_or_name <- geodato:::is_code_or_name(filter, map_name)
    if(code_or_name == "name"){
      filter <- tibble::tibble(filter = filter) |>
        gd_match(map_name) |> pull(..gd_id)
    }
    dgeo <- dgeo |> filter(id %in% filter)
  }


  lf_options <- leaflet::leafletOptions(
    zoomControl = opts$map_zoom,
    minZoom = opts$min_zoom,
    maxZoom = 18
  )

  lt <- leaflet::leaflet(dgeo,
                         option = lf_options) |>
    leaflet::addPolygons(weight = opts$theme$border_weight,
                         label = ~ ..labels,
                         color =  opts$theme$border_color,
                         fillColor = ~ ..color,
                         fillOpacity = opts$theme$topo_fill_opacity,
                         opacity = 1)


  # Add legend

  if (opts$theme$legend_show) {
    lt <- lt |> lt_legend(dgeo, opts)
  }


  # Add titles
  lt <- lt |>
    lt_titles(opts)

  #lt_background(opts$theme)
  lt <- lt |>
    lt_background(opts)

  #lflt_graticule(l$graticule) |>

  lt
}


