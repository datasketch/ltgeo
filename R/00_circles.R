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

  leaflet::leaflet(data_map,
          options =  do.call(eval(parse(text="leaflet::leafletOptions")), opts$zoom_opts)) |>
    lt_background(opts_tiles = opts$tiles_opts,
                  opts_branding = opts$branding_opts) |>
    ltg_circles(opts = c(opts$colors_opts,
                         opts$general_opts)) #|>
  #lt_legend(opts$legend_opts)

}

#' @export
lt_circles_GlnGlt <- function(data, ...) {
  var_gln <- names(data)[1]
  var_glt <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = c(var_gln, var_glt),
                                       agg_name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  lt_circles(data = data, var_gln = var_gln, var_glt = var_glt, var_num = var_num_name, ...)
}

#' @export
lt_circles_GlnGltNum <-  function(data, ...) {
  var_gln <- names(data)[1]
  var_glt <- names(data)[2]
  var_num <- names(data)[3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = c(var_gln, var_glt),
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)

  lt_circles(data = data, var_gln = var_gln, var_glt = var_glt, var_num = var_num_name, ...)
}



#' @export
lt_circles_Gnm <- function(data, ...) {
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
  lt_circles(data = data, var_gnm = var_gnm, var_num = var_num_name, ...)
}

#' @export
lt_circles_GnmNum <-  function(data, ...) {
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

  lt_circles(data = data, var_gnm = var_gnm, var_num = var_num_name, ...)
}


#' @export
lt_circles_Gcd <- function(data, ...) {
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
  lt_circles(data = data, var_gcd = var_gcd, var_num = var_num_name, ...)
}

#' @export
lt_circles_GcdNum <-  function(data, ...) {
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

  lt_circles(data = data, var_gcd = var_gcd, var_num = var_num_name, ...)
}

