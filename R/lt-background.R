
#' @keywords internal
lt_logo <- function(map,
                    opts) {
  logo_path <- NULL
  src <- "remote"
  svg <- FALSE
  logo <- opts$logo
  logo_color <- opts$logo_color
  background_color <- opts$background_color %||% "transparent"
  img_file <- opts$img_file
  width <- opts$width %||% 150
  height <- opts$height %||% 150
  position <- opts$position %||% "bottomright"
  url <- opts$url

  #Aplica cuando se llama una organizacion que se encuentra en dsthemer
  if (!is.null(logo)) {
    if (is.null(logo_color)) logo_color <- "#ffffff"
    logo_path <- dsvizopts::local_logo_path(logo, logo_color)
  }
  # Logos externos a dsthemer
  if (!is.null(img_file)) {
    logo_path <- img_file
    have_httr <- grepl("http", logo_path)
    svg <- tools::file_ext(logo_path) == "svg"
    src <- "local"
    if (have_httr) src <- "remote"
  }

  if (!svg) {
    src <- "remote"
    logo <- magick::image_read(logo_path) |>
      magick::image_background(background_color) |>
      magick::image_scale(magick::geometry_area(width = width, height = height))
    logo_data <- magick::image_write(logo, format = "png")
    logo_url <- paste0("data:image/png;base64,", base64enc::base64encode(logo_data))
  } else {
    logo_url <- logo_path
  }

  map |>
    leafem::addLogo(img = logo_url,
                    src = src,
                    url = url,
                    position = position,
                    offset.x = 0,
                    offset.y = -10,
                    height = height,
                    width = width)

}

#' @keywords internal
lt_tiles <- function(map, opts) {
  lf <- NULL
  if (is.null(opts$map_tiles) & opts$map_provider_tile == "leaflet") {
    lf <- map |>
      leaflet.extras::setMapWidgetStyle(list(background = opts$background_color))
  } else {
    switch(opts$map_provider_tile,
           "leaflet" = lf <- map |> leaflet::addProviderTiles(opts$map_tiles),
           "esri" = {
             lf <- map |> leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers[[opts$map_tiles_esri]])
             if (!is.null(opts$map_extra_layout)) {
               lf <- lf |>
                 leaflet.esri::addEsriFeatureLayer(
                   url = opts$map_extra_layout,
                   labelProperty = opts$map_name_layout)
             }
           },
           {
             lf <- map |> leaflet::addTiles(urlTemplate = opts$map_extra_layout,
                                            attribution = opts$map_name_layout)
           }
    )
  }

  lf
}

#' @export
lt_background <- function(map,
                          opts_tiles = NULL,
                          opts_branding = NULL) {

  if (is.null(map)) return()
  lf <- map

  if (!is.null(opts_tiles$map_provider_tile)) {
    lf <- lf |>
      lt_tiles(opts = opts_tiles)
  }
  if (opts_branding$branding_include) {
    lf <- lf |>
      lt_logo(opts = opts_branding)
  }
  lf

}








