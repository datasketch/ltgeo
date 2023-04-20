test_that("Filter regions", {

  opts <- list(map_name = "col_departments",
               filter = c("Amazonas", "Antioquia"),
               code_or_name = "name")
  data_geo <- data_map_draw(data = NULL,
                            dic = NULL,
                            var_geo = NULL,
                            var_num = NULL,
                            opts = opts)

  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ name,
                         color =  "#DDDDDD",
                         fillColor = "#FCFCFC",
                         fillOpacity = 1,
                         opacity = 1)

  opts <- list(map_name = "col_municipalities",
               filter = c("Medellin", "Leticia"),
               code_or_name = "name")

  data <- data.frame(depto = c("Medellin", "Leticia"),
                     value = c(1, 1, 1, 1))

  data_geo <- data_map_draw(data = data,
                            dic = NULL,
                            var_geo = "depto",
                            var_num = "value",
                            opts = opts)

  pal <- colorNumeric("YlOrRd", domain = 1)


  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ label,
                         color =  "#DDDDDD",
                         fillColor = ~pal(value),
                         fillOpacity = 1,
                         opacity = 1)
})
