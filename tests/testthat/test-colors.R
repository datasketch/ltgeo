test_that("Colors", {
  pc <- c("#b8f3ca", "#add79c", "#8dbd93", "#70a38e", "#568989", "#3f6f83", "#28557d")
  opts <- list(
    color_scale = "sequential",
    palette = pc,
    domain = 1:5,
    na_color = "#FCFCFC"
  )
  pal <- lt_palette(opts)

  data <- data.frame(depto = c("Caquetá", "Amazonas", "Vichada", "Meta", "Antioquia"),
                     value = runif(5, 1, 100))
  opts <- modifyList(opts, list(domain = data$value))
  opts_map <- list(map_name = "col_departments")
  data_geo <- data_map_draw(data = data,
                            dic = NULL,
                            var_geo = "depto",
                            var_num = "value",
                            opts = opts_map)

  pal <- lt_palette(opts)

  valor <- "value"
  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ label,
                         color =  "#DDDDDD",
                         fillColor = ~pal(value),
                         fillOpacity = 1,
                         opacity = 1)



})
