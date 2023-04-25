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

  leaflet::leaflet(data_geo$map_data) |>
    ltg_choropleth(opts = c(opts$colors_opts,
                            opts$general_opts)
                   )

}
