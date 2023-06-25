
lt_legend <- function(lt, opts, dgeo){


  if(opts$color_palette_type == "sequential"){
    pal <- colorNumeric(
      palette = opts$color_palette_sequential,
      domain = dgeo$..var,
      reverse = TRUE
    )
  }

  lt |> addLegend("bottomright", pal = pal, values = ~..var,
            title = opts$title_legend,
            labFormat = labelFormat(
              transform = function(x) sort(x, decreasing = TRUE)
            ),
            opacity = 1)

}


# lt_legend <- function(map, dgeo, opts){
#
#
#   # opts_pal <- list(color_scale = opts$extra$map_color_scale,
#   #                  palette = opts$theme$palette_colors,
#   #                  # sequential = l$palette_colors_sequential,
#   #                  # divergent = l$palette_colors_divergent,
#   #                  na_color = opts$theme$na_color,
#   #                  domain = domain,
#   #                  n_bins = l$n_bins,
#   #                  n_quantile = l$n_quantile,
#   #                  pretty = l$bins_pretty)
#
#
#   legend_position <- opts$theme$legend_position %||% "topright"
#   if (legend_position == "bottom") legend_position <- "bottomright"
#   if (legend_position == "top") legend_position <- "topright"
#
#   lt_pal <- lt_palette(dgeo, opts)
#   str(lt_pal)
#
#   map %>% leaflet::addLegend(pal = lt_pal, values = ~ ..var,
#                                   opacity = 1,
#                                   position = legend_position,
#                                   na.label = opts$preprocess$na_label,
#                                   title = opts$titles$legend_title %||% var
#                                   # ,
#                                   # labFormat = lflt_legend_format(
#                                   #   sample =l$format_num, locale = l$locale,
#                                   #   prefix = l$prefix, suffix = l$suffix,
#                                   #   between = paste0(l$suffix, " - ", l$prefix),
#                                   # )
#   )
# }




#' #' @export
#' lt_legend <- function(map, opts) {
#'
#'   if (is.null(opts$pal)) return(map)
#'
#'   if (opts$legend_discrete) {
#'     if (is.null(opts$legend_labels)) return(map)
#'     opts_discrete <- list(
#'       pal = opts$pal,
#'       legend_labels = opts$legend_labels,
#'       legend_sizes = opts$legend_sizes,
#'       legend_title = opts$legend_title,
#'       na_label = opts$na_label,
#'       legend_position = opts$legend_position,
#'       legend_opacity = opts$legend_opacity
#'     )
#'
#'     lt_discrete_legend(map, opts_discrete)
#'   } else {
#'     opts_legend <- list(
#'       map = map,
#'       position = opts$legend_position,
#'       pal = opts$pal,
#'       values = ~..domain,
#'       na.label = opts$na_label,
#'       bins = opts$legend_bins,
#'       opacity = opts$legend_opacity,
#'       labels = opts$legend_labels,
#'       labFormat = lflt_legend_format(opts = opts$legend_format),
#'       title = opts$legend_title,
#'       layerId = opts$legend_id,
#'       group = opts$legend_group
#'     )
#'
#'     if (!is.null(opts$extra_colors)) {
#'       opts_legend <- opts_legend[-3]
#'       opts_legeng$colors <- opts$extra_colors
#'     }
#'
#'     do.call("addLegend", opts_legend)
#'   }
#' }




#
# lflt_legend_format <- function(opts,
#                                transform = identity) {
#
#   prefix <- opts$prefix
#   suffix = opts$suffix
#   between = opts$between
#   sample = opts$sample
#   locale = opts$locale
#
#   formatNum <- function(x) {
#     makeup::makeup_num(transform(x), sample)#, locale = locale)
#   }
#   function(type, ...) {
#     switch(type, numeric = (function(cuts) {
#       paste0(prefix, formatNum(cuts), suffix)
#     })(...), bin = (function(cuts) {
#       n <- length(cuts)
#       cut <- cuts[-n]
#       cut_format <- cuts[-1]
#       if (n == 1) {
#         cut <- cuts
#         cut_format <- cuts
#       }
#       paste0(prefix, formatNum(cut), between, formatNum(cut_format),
#              suffix)
#     })(...), quantile = (function(cuts, p) {
#       n <- length(cuts)
#       p <- paste0(round(p * 100), "%")
#       cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
#       paste0("<span title=\"", cuts, "\">", prefix, p[-n],
#              between, p[-1], suffix, "</span>")
#     })(...), factor = (function(cuts) {
#       paste0(prefix, as.character(transform(cuts)), suffix)
#     })(...))
#   }
# }
#
#
# lt_discrete_legend <- function(map, opts){
#
#   colors <- opts$pal
#   labels <- opts$legend_labels
#   sizes <- opts$legend_sizes
#   title <- opts$legend_title
#   na.label <- opts$na_label
#   position <- opts$legend_position
#   opacity <- opts$legend_opacity
#
#   colorAdditions <- paste0(colors, ";border-radius: 50%; width:", sizes, "px; height:", sizes, "px;")
#   labelAdditions <- paste0("<div style='display: inline-block; height: ",
#                            max(sizes), "px; margin-bottom: 3px; line-height: ", max(sizes), "px; font-size: 13px; '>",
#                            labels, "</div>")
#
#   return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
#                             opacity = opacity, title = title, na.label = na.label,
#                             position = position))
# }


