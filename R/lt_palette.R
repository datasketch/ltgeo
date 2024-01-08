
#' @keywords internal
lt_palette <- function(opts) {

  color_mapping <- NULL

  l <- list(
    palette = opts$palette,
    domain = opts$domain,
    na.color = opts$na_color
  )

  if (is.null(l$domain)) return()
  if (all(is.na(l$domain))) return()

<<<<<<< HEAD:R/lt_palette.R
  if (opts$color_scale %in% c("categorical")) {
=======
  if (opts$color_scale %in% c("Category", "Custom")) {
>>>>>>> 2cd0ed03ad8b01050d2e2c7eb3267d1879236b13:R/lt-colors.R
    color_mapping <- "colorFactor"
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- "colorQuantile"
    l <- modifyList(l, list(n = opts$n_quantile))
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- "colorBin"
    l <- modifyList(l, list(bins = opts$n_bins,
                            pretty = opts$pretty))
  } else {
    color_mapping <- "colorNumeric"
  }

  color_mapping <- paste0("leaflet::", color_mapping)
<<<<<<< HEAD:R/lt_palette.R
  do.call(dstools::getfun(color_mapping), l)
=======
  do.call(getfun(color_mapping), l)
>>>>>>> 2cd0ed03ad8b01050d2e2c7eb3267d1879236b13:R/lt-colors.R
}
