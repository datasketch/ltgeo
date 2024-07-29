test_that("choropleth", {


# mapa cuando no hay datos ------------------------------------------------

  lt_choropleth( map_name = "world_countries_argentina")

  # Cambiar fondo
  lt_choropleth( map_name = "world_countries_kazakhstan",
                 map_provider_tile = NULL)
  # Borde y color de fondo
  lt_choropleth(map_name = "world_countries_kazakhstan",
                map_provider_tile = NULL,
                border_width = 1,
                border_opacity = 1,
                border_color = "#000000",
                color_palette_sequential = "transparent")


# mapa con datos ----------------------------------------------------------

  data <- data.frame(
    name = c("BOGOTA, D.C.", "CAUCA", "ANTIOQUIA", "MAGDALENA", "MAGDALENA"),
    population = c(8000000, 1500000, 6500000, 1300000, 32423)
  )

  lt_choropleth(data,
                map_name = "col_departments",
                var_geo = "name", var_num = "population")

  # Si no ingresa variables a graficar busca la primer "categoria" y hace conteo
  lt_choropleth(data,
                map_name = "col_departments")


  lt_choropleth(data,
                map_name = "col_departments",
                map_provider_tile = NULL)

  # Titulos y capas adicionales
  lt_choropleth(data,
                map_name = "col_departments",
                var_geo = "name", var_num = "population", agg = "mean",
                title = "esto es un titulo", title_align = "right",
                map_name_layers = c("col_departments_pacifico", "col_departments_andina"))


  # Formato numerico
  lt_choropleth(data,
                map_name = "col_departments",
                var_geo = "name", var_num = "population",
                format_sample_num = "1,000.2",
                map_name_layers = c("col_departments_pacifico", "col_departments_andina"))

})
