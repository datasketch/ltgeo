test_that("Background Map", {

  library(leaflet)

  bg <- "#082034"
  opts <- list(
    map_tiles = NULL,
    map_provider_tile = "leaflet",
    background_color = bg
  )
  opts_logo <- list(
    branding_include = FALSE
  )
  l <- leaflet() |>
    addTiles() |>
    lt_background(opts_tiles = opts,
                  opts_branding = opts_logo)
  l

  expect_equal(bg, get_leaflet_option(l, "background"))

})
