

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

  opts <- dsopts_merge(...)

  # opts <- dsopts_merge(opts)
  #opts <- dsopts::dsopts_merge()

  dd <- data

  l <- prep_geo(dd, map_name, var = var, opts)
  dgeo <- l$dgeo
  opts <- l$opts


  # Filter regions
  if(!is.null(filter)) {
    code_or_name <- geodato:::is_code_or_name(filter, map_name)
    if(code_or_name == "name"){
      filter <- tibble::tibble(filter = filter) |>
        gd_match(map_name) |> pull(..gd_id)
    }
    dgeo <- dgeo |> filter(id %in% filter)
  }

  opts$legend_discrete <- FALSE
  if (is.character(dd[[var]])) {
    opts$legend_discrete <- TRUE
    cat_order <- if (all(is.na(opts$legend_cat_order))) { NULL } else { opts$legend_cat_order }
    opts$legend_labels <- cat_order %||% unique(dgeo[[var]])
  }

  lf_options <- leaflet::leafletOptions(
    zoomControl = opts$zoom_show
    # minZoom = opts$zoom_min,
    # maxZoom = opts$zoom_max
  )

  lt <- leaflet::leaflet(dgeo,
                         option = lf_options)
  if(!opts$map_popup){
    lt <- lt |> leaflet::addPolygons(weight = opts$border_width,
                                     stroke = TRUE,
                                     label = ~ ..labels,
                                     color =  opts$border_color,
                                     fillColor = ~ ..color,
                                     fillOpacity = opts$opacity,
                                     opacity = 1)
  } else{
    lt <- lt |> leaflet::addPolygons(weight = opts$border_width,
                                     stroke = TRUE,
                                     #label = ~ ..labels,
                                     color =  opts$border_color,
                                     fillColor = ~ ..color,
                                     fillOpacity = opts$opacity,
                                     opacity = 1,
                                     popup = ~ ..popup)
  }

  map_name_layers <- if (opts$map_name_layers == "NA") { NULL } else { opts$map_name_layers }
  if (!is.null(map_name_layers)) {
    topoData <- geojsonio::topojson_json(geodato::gd_tj(map_name_layers))
    lt <- lt |>
      leaflet::addTopoJSON(
        topojson = topoData,
        weight = 1,#opts$map_name_layers_params$weight,
        opacity = 1,#opts$map_name_layers_params$opacity,
        fill = F,
        #fillColor = "transparent",
        color = opts$map_name_layers_params$color
        )
  }


  # Add titles
  lt <- lt |>
    lt_titles(opts)

  opts_tiles <- list(
    map_tiles = if (opts$map_tiles == "NA") { NULL } else { opts$map_tiles },
    map_provider_tile = opts$map_provider_tile,
    background_color = opts$background_color,
    map_tiles_esri = opts$map_tiles_esri
  )
  lt <- lt |>
    lt_background(opts_tiles = opts_tiles)

  # Add legend
  if (opts$legend_show) {
    color_scale <- ifelse(is.na(opts$color_palette_type), "sequential", opts$color_palette_type)
    if (color_scale == "sequential") {
      opts_lg <- list(
        domain = unique(setdiff(dgeo$..var, NA)),
        palette = opts[[paste0("color_palette_", color_scale)]],
        na_color = opts$na_color,
        color_scale = color_scale
      )
      opts$pal <- lt_palette(opts_lg)
    } else {
      dgeo[[var]] <- factor(dgeo[[var]], levels = opts$legend_labels)
      dgeo <- dgeo[order(dgeo[[var]]), ]
      opts$pal <- unique(dgeo$..color)
    }

    lt <- lt |> lt_legend(opts)
  }

  #lflt_graticule(l$graticule) |>

  lt
}


