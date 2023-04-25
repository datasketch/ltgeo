#' @export
lt_choropleth <- function(data = NULL,
                          dic = NULL,
                          var_gnm = NULL,
                          var_gcd = NULL,
                          var_num = NULL, ...) {

  if (is.null(var_gnm) & is.null(var_gcd))
    stop("You must enter at least one geographic variable")

  frType <- frtype_viz(var_gnm = var_gnm,
                       var_gcd = var_gcd,
                       var_num = var_num)
  var_geo <- var_gnm %||% var_gcd
  opts <- plot_opts(viz = "choropleth", frType = frType, ...)

  data_geo <- data_map_draw(data = data,
                            dic = dic,
                            var_geo = var_geo,
                            var_num = var_num,
                            opts = opts$data_opts)
  opts$colors_opts$domain <- setdiff(data_geo$map_data$..domain, NA)
  pal <- lt_palette(opts$colors_opts)
  opts$colors_opts$pal <- pal
  opts$legend_opts$pal <- pal
  leaflet(data_geo$map_data) |>
    lt_background(opts_tiles = opts$tiles_opts,
                  opts_branding = opts$branding_opts) |>
    ltg_choropleth(opts = c(opts$colors_opts,
                            opts$general_opts)) |>
    lt_legend(opts$legend_opts)

}
