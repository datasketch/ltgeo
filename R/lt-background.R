
#' @keywords internal
lt_logo <- function(map,
                    logo = NULL,
                    logo_color = NULL,
                    background_color = "transparent",
                    img_file = NULL,
                    width = NULL,
                    height = NULL,
                    position = "bottomright",
                    url = NULL) {
  logo_path <- NULL
  src <- "remote"
  svg <- FALSE
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
