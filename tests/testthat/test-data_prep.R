test_that("data_prep", {

  # WITH DATA
  data <- data.frame(
    name = c("BOGOTA, D.C.", "CAUCA", "ANTIOQUIA", "MAGDALENA", "MAGDALENA"),
    population = c(8000000, 1500000, 6500000, 1300000, 32423)
  )

  data_geo <- data_prep(data,
                        var_geo = "name",
                        var_num = "population",
                        opts = list(map_name = "col_departments")
                        )






})
