
lflt_legend_format <- function(opts,
                               transform = identity) {

  prefix <- opts$prefix
  suffix = opts$suffix
  between = opts$between
  sample = opts$sample
  locale = opts$locale

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

#' @export
lt_legend <- function(map, opts) {

  #if (!opts$legend_show) return(map)

  opts_legend <- list(
    map = map,
    position = opts$legend_position,
    pal = opts$pal,
    values = ~..domain,
    na.label = opts$na_label,
    bins = opts$legend_bins,
    opacity = opts$legend_opacity,
    labels = opts$legend_labels,
    labFormat = lflt_legend_format(opts = opts$legend_format),
    title = opts$legend_title,
    layerId = opts$legend_id,
    group = opts$legend_group
  )

  if (!is.null(opts$extra_colors)) {
    opts_legend <- opts_legend[-3]
    opts_legeng$colors <- opts$extra_colors
  }

  do.call("addLegend", opts_legend)
}

# lflt_legend_bubbles <- function(map, colors, labels, sizes,
#                                 title, na.label, position, opacity){
#   colorAdditions <- paste0(colors, ";border-radius: 50%; width:", sizes, "px; height:", sizes, "px;")
#   labelAdditions <- paste0("<div style='display: inline-block; height: ",
#                            max(sizes), "px; margin-bottom: 3px; line-height: ", max(sizes), "px; font-size: 13px; '>",
#                            makeup::makeup_num(labels), "</div>")
#
#   return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
#                             opacity = opacity, title = title, na.label = na.label,
#                             position = position))
# }
