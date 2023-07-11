lflt_legend_format <- function(opts,
                               transform = identity) {


  #format_num <- makeup::which_num_format(opts$format_sample_num)
  prefix <- NULL
  suffix <- NULL
  between <- opts$between
  sample <- opts$format_sample_num
  locale <- opts$locale

  formatNum <- function(x) {
    makeup::makeup_num(transform(x), sample)#, locale = locale)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      cut <- cuts[-n]
      cut_format <- cuts[-1]
      if (n == 1) {
        cut <- cuts
        cut_format <- cuts
      }
      paste0(prefix, formatNum(cut), between, formatNum(cut_format),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}


lt_discrete_legend <- function(map, opts){

  colors <- opts$pal
  labels <- opts$legend_labels
  sizes <- opts$legend_sizes
  title <- opts$legend_title
  na.label <- opts$na_label
  position <- opts$legend_position
  opacity <- opts$legend_opacity %||% 0.7

  colorAdditions <- paste0(colors, ";border-radius: 50%; width:", sizes, "px; height:", sizes, "px;")
  labelAdditions <- paste0("<div style='display: inline-block; height: ",
                           max(sizes), "px; margin-bottom: 3px; line-height: ", max(sizes), "px; font-size: 13px; '>",
                           labels, "</div>")

  return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
                            opacity = opacity, title = title, na.label = na.label,
                            position = position))
}

#' @export
lt_legend <- function(map, opts) {

  if (is.null(opts$pal)) return(map)

  if (opts$legend_discrete) {
    if (is.null(opts$legend_labels)) return(map)
    opts_discrete <- list(
      pal = opts$pal,
      legend_labels = opts$legend_labels,
      legend_sizes = opts$legend_sizes,
      legend_title = opts$legend_title,
      na_label = opts$na_label,
      legend_position = opts$legend_position,
      legend_opacity = opts$legend_opacity
    )

    lt_discrete_legend(map, opts_discrete)
  } else {
    opts_legend <- list(
      map = map,
      #position = opts$legend_position,
      pal = opts$pal,
      values = ~..var,
      na.label = opts$na_label,
      bins = opts$color_bins_n,
      opacity = opts$legend_opacity,
      # labels = opts$legend_labels,
      # labFormat = lflt_legend_format(opts = opts$legend_format),
       title = opts$legend_title#,
      # layerId = opts$legend_id,
      # group = opts$legend_group
    )

    if (!is.null(opts$extra_colors)) {
      opts_legend <- opts_legend[-3]
      opts_legeng$colors <- opts$extra_colors
    }

    do.call("addLegend", opts_legend)
  }
}
