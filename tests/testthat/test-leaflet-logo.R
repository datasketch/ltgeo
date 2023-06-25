test_that("Logo in map", {

  l <- leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = sys_ltgeo("leaflet-logo.png"),
      width = 375,
      height = 125)
    )
  l


  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = "https://www.r-project.org/logo/Rlogo.svg",
      width = 70,
      height = 70,
      position = "topright")
    )

  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      img_file = sys_ltgeo("Rlogo.svg"),
      width = 300,
      height = 50
    )
    )


  # requiere paquete dsthemer
  # library(dsthemer)
  leaflet() |>
    addTiles() |>
    lt_logo(opts = list(
      logo = "datasketch",
      logo_color = "#ffffff",
      width = 200,
      height = 80
    )
    )

})
