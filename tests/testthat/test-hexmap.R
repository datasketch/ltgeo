test_that("Hexmap function", {
  data <- quakes
  lt_hexmap(data,
            map_basic = FALSE,
            var_gln = "long",
            var_glt = "lat",
            map_hex_opacity = 1,
            map_legend_discrete = TRUE,
            legend_labels = c("mayor", "menor"),
            palette_colors = c("yellow", "red"))

  n <- 1000
  df <- data.frame(lng = rnorm(n, -93.65, .01),
                   lat = rnorm(n, 42.0285, .01))

  lt_hexmap_GlnGlt(df,
                   map_basic = FALSE,
                   map_hex_opacity = 1,
                   map_legend_discrete = FALSE,
                   legend_labels = c("mayor", "menor"),
                   palette_colors = c("yellow", "red"))

})
