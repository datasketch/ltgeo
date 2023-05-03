test_that("Circle function", {
  data <- quakes
  lt_circles(data,
             var_gln = "long",
             var_glt = "lat",
             var_num = "mag",  map_basic = FALSE)

  data <- data |> select(long, everything())
  lt_circles_GlnGlt(data, map_basic = FALSE)
  lt_circles_GlnGltNum(data, map_basic = FALSE)

  # map cluster solo funciona si tiene un tile
  lt_circles(data,
             var_gln = "long",
             var_glt = "lat",
             var_num = "mag",
             map_tiles = "CartoDB.Positron",
             map_basic = FALSE,
             map_cluster = TRUE)


  data <- data.frame(depto = c("Caquetá", "Amazonas", "Vichada", "Meta", "Antioquia"),
                     value = runif(5, 1, 100))
  lt_circles(data,
             var_num = "value",
             map_name = "col_departments")


  lt_circles_GnmNum(data, map_name = "col_departments")

})
