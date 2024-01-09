test_that("General functions to create maps", {

  # create choropleth


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

  opts_map <- list(map_name = "col_departments")
  data_geo <- data_map_draw(data = data,
                            dic = NULL,
                            var_geo = "depto",
                            var_num = "value",
                            opts = opts_map)


  leaflet::leaflet(data_geo$map_data) |>
    ltg_choropleth(opts = opts_colors)




  data <- quakes
  data <- data |> rename(lon = long)
  data_geo <- data_map_draw(data = data,
                            dic = NULL,
                            var_geo = c("lon", "lat"),
                            var_num = "mag",
                            opts = list(
                              data_rename = FALSE,
                              map_name = "world_countries_world")
  )


  opts_circles <- list(
    map_basic = FALSE,
    cluster_add = TRUE,
    cluster_opts = list(showCoverageOnHover = TRUE,
                        removeOutsideVisibleBounds = F),
    extra_data = data_geo$map_data$data,
    map_circle_color = "#28557d",
    map_radius = 5
  )
  leaflet::leaflet(data_geo$map_data$dgeo) |>
    addTiles() |>
    ltg_circles(opts = opts_circles)


  opts_hex <- list(
    map_basic = FALSE,
    extra_data = data_geo$map_data$data,
    map_radius = 5
  )

  leaflet::leaflet(data_geo$map_data$dgeo) |>
    addTiles() |>
    ltg_hexmap(opts = opts_hex)


})
