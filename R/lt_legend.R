
lt_legend <- function(map, dgeo, opts){


  # opts_pal <- list(color_scale = opts$extra$map_color_scale,
  #                  palette = opts$theme$palette_colors,
  #                  # sequential = l$palette_colors_sequential,
  #                  # divergent = l$palette_colors_divergent,
  #                  na_color = opts$theme$na_color,
  #                  domain = domain,
  #                  n_bins = l$n_bins,
  #                  n_quantile = l$n_quantile,
  #                  pretty = l$bins_pretty)


  legend_position <- opts$theme$legend_position %||% "topright"
  if (legend_position == "bottom") legend_position <- "bottomright"
  if (legend_position == "top") legend_position <- "topright"

  lt_pal <- lt_palette(dgeo, opts)
  str(lt_pal)

  map %>% leaflet::addLegend(pal = lt_pal, values = ~ ..var,
                                  opacity = 1,
                                  position = legend_position,
                                  na.label = opts$preprocess$na_label,
                                  title = opts$titles$legend_title %||% var
                                  # ,
                                  # labFormat = lflt_legend_format(
                                  #   sample =l$format_num, locale = l$locale,
                                  #   prefix = l$prefix, suffix = l$suffix,
                                  #   between = paste0(l$suffix, " - ", l$prefix),
                                  # )
  )
}

