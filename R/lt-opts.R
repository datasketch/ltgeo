plot_opts <- function(viz = NULL, frType = NULL, ...) {
  if (is.null(viz)) return()
  opts <- dsvizopts::merge_dsviz_options(...)
  plot_type <- viz
  palette_colors <- opts$theme$palette_colors %||% opts$theme$palette_colors_sequential

  titles_opts <- list(
    title = opts$titles$title,
    subtitle = opts$titles$subtitle,
    caption = opts$titles$caption,
    caption_show = !is.null(opts$titles$caption)
  )
  tiles_opts <- list(
    map_tiles = opts$theme$map_tiles,
    map_provider_tile = opts$theme$map_provider_tile,
    background_color = opts$theme$background_color,
    map_tiles_esri = opts$theme$map_tiles_esri,
    map_extra_layout = opts$theme$map_extra_layout,
    map_name_layout = opts$theme$map_name_layout,
  )
  branding_opts = list(
    logo = opts$theme$logo,
    logo_color = opts$theme$logo_color %||% "#FFFFFF",
    background_color = opts$theme$branding_background_color %||% "transparent",
    img_file = opts$theme$logo_file,
    width = opts$theme$logo_width,
    height = opts$theme$logo_height,
    position = opts$theme$branding_position %||% "bottomright",
    url = opts$theme$logo_url,
    branding_include = opts$theme$branding_include
  )
  colors_opts <- list(palette = palette_colors,
                      na_color = opts$theme$na_color,
                      color_scale = opts$map$map_color_scale,
                      n_quantile = opts$map$map_quantile,
                      n_bins = opts$map$map_bins,
                      pretty = opts$map$map_bins_pretty
                      )
  legend_opts <- list(legend_position = opts$theme$legend_position,
                      na_label = opts$prep$na_label,
                      legend_bins = opts$map$map_bins,
                      legend_opacity = opts$map$map_opacity,
                      legend_labels = opts$theme$legend_labels,
                      legend_format = list(prefix = opts$prep$prefix_num,
                                           suffix = opts$prep$suffix_num,
                                           between = opts$theme$legend_sep,
                                           sample = opts$prep$format_sample_num,
                                           locale = NULL),
                      legend_title = opts$titles$legend_title,
                      legend_id = opts$theme$legend_id,
                      legend_group = opts$theme$legend_group
                      )
  data_opts <- list(map_name = opts$map$map_name,
                    tooltip_template = opts$chart$tooltip_template,
                    na_label = opts$prep$na_label,
                    format_sample_num = opts$prep$format_sample_num,
                    prefix_num = opts$prep$prefix_num,
                    suffix_num = opts$prep$suffix_num,
                    si_prefix = opts$prep$si_prefix %||% FALSE,
                    format_sample_cat = opts$prep$format_sample_cat,
                    format_sample_dat = opts$prep$format_sample_dat,
                    color_by = opts$prep$color_by,
                    filter = opts$map$filter_region
                    )

  general_opts <- list(layer_id = opts$map$map_layer,
                   group = opts$map$map_group,
                   stroke = opts$map$map_stroke,
                   color = opts$theme$border_color,
                   weight = opts$theme$border_weight,
                   popup = opts$map$map_popup,
                   popup_options = opts$map$map_popup_options,
                   label_options = opts$map$map_label_options

  )

  if (viz == "choropleth") {
    choropleth_opts <- list(
      opacity = opts$map$map_opacity,
      fill = opts$map$map_fill %||% TRUE,
      fill_opacity = opts$map$map_fill_opacity %||% 1,
      dashArray = opts$map$map_dash,
      smooth_factor = opts$map$map_smooth %||% 1,
      no_clip = opts$map$map_noclip %||% FALSE,
      highlight_options = opts$map$map_highlight
    )
    general_opts <- c(general_opts, choropleth_opts)
  }


  list(titles = titles_opts,
       data_opts = data_opts,
       general_opts = general_opts,
       colors_opts = colors_opts,
       legend_opts = legend_opts,
       branding_opts = branding_opts,
       tiles_opts = tiles_opts
       #theme = opts$theme
       )

}
