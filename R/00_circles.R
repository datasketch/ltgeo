#' @export
lt_circles <- function(data = NULL,
                       dic = NULL,
                       var_gln = NULL,
                       var_glt = NULL,
                       var_gnm = NULL,
                       var_gcd = NULL,
                       var_num = NULL, ...) {

  var_geo <- c(var_gln, var_glt)
  var_coor <- TRUE
  if (is.null(var_geo)) {
    var_coor <- FALSE
    var_geo <- var_gnm %||% var_gcd %||% geodato::parse_col(data)
  }

  frType <- frtype_viz(var_gnm = var_gnm,
                       var_gcd = var_gcd,
                       var_gln = var_gln,
                       var_glt = var_glt,
                       var_num = var_num)

  opts <- plot_opts(viz = "circles", frType = frType, ...)
  opts$data_opts$data_rename <- var_coor
  data_geo <- data_map_draw(data = data,
                            dic = dic,
                            var_geo = var_geo,
                            var_num = var_num,
                            opts = opts$data_opts)

  if (is.null(data_geo$map_data$data)) {
    opts$colors_opts$domain <- setdiff(data_geo$map_data$..domain, NA)
    opts$general_opts$extra_data <- NULL
    data_map <- data_geo$map_data
  } else {
    opts$colors_opts$domain <- setdiff(data_geo$map_data$data$..domain, NA)
    opts$general_opts$extra_data <- data_geo$map_data$data
    data_map <- data_geo$map_data$dgeo
  }
  pal <- lt_palette(opts$colors_opts)
  opts$colors_opts$pal <- pal
  opts$legend_opts$pal <- pal

  leaflet(data_map) |>
    lt_background(opts_tiles = opts$tiles_opts,
                  opts_branding = opts$branding_opts) |>
    ltg_circles(opts = c(opts$colors_opts,
                         opts$general_opts)) #|>
  #lt_legend(opts$legend_opts)

}
