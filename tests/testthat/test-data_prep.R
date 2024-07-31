test_that("data_prep", {

  # Gnm o Gcd
  data <- data.frame(
    name = c("BOGOTA, D.C.", "CAUCA", "ANTIOQUIA", "MAGDALENA", "MAGDALENA"),
    population = c(8000000, 1500000, 6500000, 1300000, 32423)
  )

  data_geo <- data_prep(data,
                        var_geo = "name",
                        var_num = "population",
                        map_name = "col_departments"
  )


  # Gln y Glt
  data <- data.frame(
    Latitud = runif(30, min = -90, max = 90),
    Longitud = runif(30, min = -180, max = 180)
  )

  data_coor <- data_prep_bubbles(data,
                                var_gln = "Longitud",
                                var_glt = "Latitud",
                                map_name = "col_departments")
  data_coor <- data_coor$data_geo

  data <- data.frame(
    Latitud = runif(30, min = -90, max = 90),
    Longitud = runif(30, min = -180, max = 180),
    letras = c(rep("a", 10), rep("b", 20))
  )

  data_coor <- data_prep_bubbles(data,
                                var_gln = "Longitud",
                                var_glt = "Latitud",
                                map_name = "col_departments",
                                tooltip_add_unique_cats = "letras")
  data_coor <- data_coor$data_geo

})
