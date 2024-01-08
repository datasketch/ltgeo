#' @export
lt_choropleth <- function(data = NULL,
                          dic = NULL,
                          var_gnm = NULL,
                          var_gcd = NULL,
                          var_num = NULL, ...) {


  var_geo <- NULL
  if (!is.null(data)) {
    var_geo <- var_gnm %||% var_gcd %||% geodato::parse_col(data)
    if (is.null(var_geo)) stop("You must enter at least one geographic variable")
    frType <- frtype_viz(var_gnm = var_gnm,
                         var_gcd = var_gcd,
                         var_num = var_num)
  }

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

  leaflet::leaflet(data_geo$map_data,
          options =  do.call(eval(parse(text="leaflet::leafletOptions")), opts$zoom_opts)) |>
    lt_background(opts_tiles = opts$tiles_opts,
                  opts_branding = opts$branding_opts) |>
    ltg_choropleth(opts = c(opts$colors_opts,
                            opts$general_opts)) |>
    lt_legend(opts$legend_opts)

}


#' @export
lt_choropleth_Gnm <- function(data, ...) {
  var_gnm <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_gnm,
                                       agg_name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  lt_choropleth(data = data, var_gnm = var_gnm, var_num = var_num_name, ...)
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


#' @export
lt_choropleth_Gcd <- function(data, ...) {
  var_gcd <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_gcd,
                                       agg_name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  lt_choropleth(data = data, var_gcd = var_gcd, var_num = var_num_name, ...)
}

#' @export
lt_choropleth_GcdNum <-  function(data, ...) {
  var_gcd <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_gcd,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)

  lt_choropleth(data = data, var_gcd = var_gcd, var_num = var_num_name, ...)
}
