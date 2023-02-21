test_that("Maps of the world", {


  library(tidyverse)

  map_name <- "world_countries"

  data <- data.frame(
    country = c("Colombia", "Argentina", "Brasil", "Ecuador", "Perú"),
    cantidad = 1:5,
    categoria = c("Amarillo", "Azul", "Amarillo", "Amarillo", "Rojo")
  )

  var <- "cantidad"
  opts_list <- list(
    tooltip = "Hola {country}, Cantidad: {cantidad}, Cat: {categoria}"
  )
  tooltip <- opts_list$tooltip

  lt_choropleth(data, map_name = map_name, var = var)

  lt_choropleth(data, map_name = map_name, var = var, opts = opts_list)
  var <- "categoria"
  lt_choropleth(data, map_name = map_name, var = var, opts = opts_list)



})
