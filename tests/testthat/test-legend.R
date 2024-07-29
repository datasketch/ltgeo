test_that("legend format", {
  opts <- list(format_sample_num = "$1,000.00")
  formatter <- lt_legend_format(opts)

  countries_data <- data.frame(
    country = c("China", "Russia", "Perú", "Brazil", "Algeria", "Pakistan"),
    value = c(12442373, 11034555, 8443675, 6772291, 4681087, 4486679)
  )

  lt_choropleth(data = countries_data,
                map_name = "world_countries",
                var_num = "value",
                map_provider_tile = NULL
                )


})
