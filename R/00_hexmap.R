#' @export
lt_hexmap <- function(data = NULL,
                      dic = NULL,
                      var_gln = NULL,
                      var_glt = NULL,
                      var_gnm = NULL,
                      var_gcd = NULL, ...) {

  var_geo <- c(var_gln, var_glt)
  var_coor <- TRUE
  if (is.null(var_geo)) {
    var_coor <- FALSE
    var_geo <- var_gnm %||% var_gcd %||% geodato::parse_col(data)
  }

  frType <- frtype_viz(var_gnm = var_gnm,
                       var_gcd = var_gcd,
                       var_gln = var_gln,
                       var_glt = var_glt)

  opts <- plot_opts(viz = "hexmap", frType = frType, ...)

  opts$data_opts$data_rename <- var_coor
  data_geo <- data_map_draw(data = data,
                            dic = dic,
                            var_geo = var_geo,
                            opts = opts$data_opts)

  if (is.null(data_geo$map_data$data)) {
    opts$general_opts$extra_data <- NULL
    data_map <- data_geo$map_data
  } else {
    opts$general_opts$extra_data <- data_geo$map_data$data
    data_map <- data_geo$map_data$dgeo
  }

  leaflet::leaflet(data_map,
          options =  do.call(eval(parse(text="leaflet::leafletOptions")), opts$zoom_opts)) |>
    lt_background(opts_tiles = opts$tiles_opts,
                  opts_branding = opts$branding_opts) |>
    ltg_hexmap(opts = opts$general_opts) |>
    lt_legend(opts$legend_opts)

}

#' @export
lt_hexmap_GlnGlt <- function(data, ...) {
  var_gln <- names(data)[1]
  var_glt <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  lt_hexmap(data = data, var_gln = var_gln, var_glt = var_glt, ...)
}


#' #' @export
#' lt_hexmap_Gnm <- function(data, ...) {
#'   var_gnm <- names(data)[1]
#'   opts_prep <- dataprep_opts(...)
#'
#'   lt_hexmap(data = data, var_gnm = var_gnm, ...)
#' }




#' #' @export
#' lt_hexmap_Gcd <- function(data, ...) {
#'   var_gcd <- names(data)[1]
#'   opts_prep <- dataprep_opts(...)
#'   lt_hexmap(data = data, var_gcd = var_gcd, ...)
#' }


