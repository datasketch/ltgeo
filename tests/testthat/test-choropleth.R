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
    16, "Alta Verapaz", 2,"mayor",
    15, "Baja Verapaz", 4,"menor",
    4, "Chimaltenango", 5,"mayor",
    9, "Quetzaltenango", 6,"menor"
  )


  lt_choropleth(data, map_name = "gtm_departments",
                var_gnm = "departamento",
                var_num = "cantidad",
                map_tiles = "CartoDB.VoyagerOnlyLabels")

  lt_choropleth(data, map_name = "gtm_departments",
                var_gcd = "id",
                var_num = "cantidad",
                map_tiles = "CartoDB.VoyagerOnlyLabels")


})
