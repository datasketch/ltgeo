test_that("multiplication works", {

  library(tidyverse)

  data <- data.frame(mcpio = c("Dagua", "El Encanto", "La Chorrera", "Abejorral"),
                     valor = runif(4, 100, 500))

  lt_choropleth(data, var = "valor",
                map_name = "col_municipalities",
                map_name_layers = "col_departments",
                map_name_layers_params = list(color = "#000000"),
                caption = "esto son los pie de pagina",
                title = "esto es un titulo")


  df <- read_csv("tmp/boyaca-especies.csv")

  map_name <- "col_municipalities_boyaca"
  var <- "value"
  opts <- list(
    background_color = "#ffffff",
    tooltip_template = "<b>{value} especies</b><br><i>{label}</i><br><br>
                        <a href='https://deploy-preview-1--cifras-biodiversidad.netlify.app/boyaca/{label}' target='_blank'>Ver más</a>",
    zoom_show = TRUE,
    border_width = 1,
    border_color = "white",
    map_tiles = NULL,
    map_popup = TRUE,
    legend_show = TRUE,
    title_legend = "Especies"
  )

  data <- df
  lt <- lt_choropleth(df, map_name = map_name, var = var, opts = opts)
  lt

})
