test_that("Choropleth function", {

  data <- data.frame(depto = c("Caquetá", "Amazonas", "Vichada",
                               "Meta", "Guainía"),
                     value = runif(5, 1, 100))
  lt_choropleth(data = data,
                var_gnm = "depto",
                var_num = "value",
                map_name = "col_departments")


  lt_choropleth_GnmNum(data,  map_name = "col_departments")

  # Data with codes 9 instead of "09".
  data <- tibble::tribble(
    ~id, ~departamento, ~cantidad, ~categoria,
    "16", "Alta Verapaz", 2,"mayor",
    "15", "Baja Verapaz", 4,"menor",
    "04", "Chimaltenango", 5,"mayor",
    "09", "Quetzaltenango", 6,"menor"
  )

  gd_match(data, map_name = "gtm_departments")
  gd_match_codes(data, map_name = "gtm_departments", col = "id")
  lt_choropleth(data, map_name = "gtm_departments",
                var_gnm = "departamento",
                var_num = "cantidad",
                map_tiles = "CartoDB.VoyagerOnlyLabels")

  data <- data[,-2]
  lt_choropleth(data, map_name = "gtm_departments",
                var_gcd = "id",
                var_num = "cantidad",
                map_tiles = "CartoDB.VoyagerOnlyLabels")

  data <- data |> select(id, cantidad, everything())
  lt_choropleth_GcdNum(data, map_name = "gtm_departments")

})
