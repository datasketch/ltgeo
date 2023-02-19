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

  #opts <- dsvizopts::dsviz_defaults(flat = TRUE)
  #opts$title <- "Hello, Guatemala"
  #opts$subtitle <- "These are my departments"

  filter <- NULL
  opts <- NULL
  opts <- list(
    title = "Hello, Guatemala",
    subtitle = "These are my departments"
  )
  var <- "cantidad"
  lt_choropleth(data, map_name = map_name, var = var, opts = opts)
  lt_choropleth(data, map_name = map_name, var = var,
                title = "Hello world", title_color = "#AB9699")

  var <- "categoria"
  lt_choropleth(data, map_name = map_name, var = var,
                title = "Hello world", title_color = "#AB9699")


})


