test_that("Logo in map", {

  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = "tests/testthat/logo/dark.png",
      width = 375,
      height = 125
    )
    )

  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = "https://www.r-project.org/logo/Rlogo.svg",
      width = 70,
      height = 70,
      position = "topright"
    )
    )

  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = "tests/testthat/logo/ds_logo.svg",
      width = 200,
      height = 150
    )
    )


  # requiere paquete dsthemer
  # library(dsthemer)
  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      logo = "datasketch",
      logo_color = "#ffffff",
      width = 375,
      height = 125
    )
    )

})
