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

#' @export
lt_choropleth_GnmNum <-  function(data, ...) {
  var_gnm <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_gnm,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)

  lt_choropleth(data = data, var_gnm = var_gnm, var_num = var_num_name, ...)
}



