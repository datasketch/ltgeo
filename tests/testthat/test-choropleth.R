test_that("Choropleth function", {
  data <- data.frame(depto = c("Caquetá", "Amazonas", "Vichada",
                               "Meta", "Guainía"),
                     value = runif(5, 1, 100))
  lt_choropleth(data = data,
                var_gnm = "depto",
                var_num = "value",
                map_name = "col_departments")

})
