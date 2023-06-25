test_that("multiplication works", {

  map_name <- "gtm_departments"


  # Data with codes 9 instead of "09".
  data <- tibble::tribble(
    ~id, ~departamento, ~cantidad, ~categoria,
    16, "Alta Verapaz", 2,"mayor",
    15, "Baja Verapaz", 4,"menor",
    4, "Chimaltenango", 5,"mayor",
    9, "Quetzaltenango", 6,"menor"
  )
  dd <- data
  opts <- dsopts::dsopts_merge()
  var <- "cantidad"


  opts <- list(
    title = "Hello, Guatemala",
    subtitle = "These are my departments"
  )
  lt_choropleth(data, map_name = map_name, var = var, opts = opts)


  lt_choropleth(data, map_name = map_name, var = var,
                title = "Hello world", title_color = "#AB9699")

  var <- "categoria"
  lt_choropleth(data, map_name = map_name, var = var,
                title = "Hello world", title_color = "#AB9699")


  ###

  library(tidyverse)

  map_name <- "gtm_municipalities"

  data <- readr::read_csv("tmp/ojoconmipiso-mapa-elecciones-2023.csv")

  data <- data |>
    rename(habitantes = `Número de habitantes`,
           desnutricion = `Desnutrición crónica`,
           candidatos = `Conoce a los candidatos`,
           link = enlace)

  tpl <- "
  Número de habitantes: {habitantes}<br>
  Desnutrición: {desnutricion}<br>
  Candidatos: {candidatos}<br>
  {link}
  "
  data$..popup <- glue::glue_data(data, tpl)
  data$..popup <- map(data$..popup, htmltools::HTML)
  #data$..tooltip <- data$..popup
  #data$..labels <- data$popup


  bg_color <- "#e8e6de"
  na_color <- "#828282"
  paleta <- c("#f14012", "#c64d22", "#eb6b37", "#26201e")

  opts <- list(
    background_color = bg_color,
    palette_colors_sequential = paleta[1],
    title = "Explora la información sobre candidatos",
    na_color = na_color,
    legend_show = TRUE
  )

  l <- get_opts(opts)
  l$theme$legend_show

  var <- "candidatos"
  lt_choropleth(data, map_name = map_name, var = var, opts = opts)






})


