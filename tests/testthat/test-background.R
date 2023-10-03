test_that("Background Map", {
  opts <- list(
    map_tiles = NULL,
    map_provider_tile = "leaflet",
    background_color = "#000"
  )
  opts_logo <- list(
    branding_include = FALSE
  )
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    lt_background(opts_tiles = opts,
                  opts_branding = opts_logo)
})
