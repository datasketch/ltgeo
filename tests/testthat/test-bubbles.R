test_that("Bubble map", {
  lt_bubbles(map_name = "col_departments")
  data <- data.frame(
    Latitud = runif(30, min = -90, max = 90),
    Longitud = runif(30, min = -180, max = 180)
  )
  lt_bubbles(data = data,  var_gln = "Longitud",
             var_glt = "Latitud")
})
