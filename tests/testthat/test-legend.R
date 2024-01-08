test_that("legend in map", {

  data <- data.frame(depto = c("Caquetá", "Amazonas", "Vichada", "Meta", "Antioquia"),
                     value = runif(5, 1, 100))

  pc <- c("#b8f3ca", "#add79c", "#8dbd93", "#70a38e", "#568989", "#3f6f83", "#28557d")
  opts_colors <- list(
    color_scale = "sequential",
    palette = pc,
    domain = data$value,
    na_color = "#FCFCFC",
    weight = 1,
    color =  "#DDDDDD",
    fill_opacity = 1,
    opacity = 1
  )
  opts_colors$pal <- lt_palette(opts_colors)
  opts_legend <- list(
    position = "bottomleft",
    pal = lt_palette(opts_colors),
    na_label = "NA",
    opacity = 1,
    extra_colors = NULL,
    legend_bins = 4,
    legend_title = " ",
    legend_labels = NULL,
    legend_id = NULL,
    legend_group = NULL,
    legend_format = list(
      prefix = NULL,
      suffix = NULL,
      between = " &ndash; ",
      sample = "1,234.1"
    )
  )
  opts_map <- list(map_name = "col_departments")
  data_geo <- data_map_draw(data = data,
                            dic = NULL,
                            var_geo = "depto",
                            var_num = "value",
                            opts = opts_map)


  leaflet::leaflet(data_geo$map_data) |>
    ltg_choropleth(opts = opts_colors) |>
    lt_legend(opts = opts_legend)
})
