test_that("tile in map", {


  # Mapa con fondo negro
  opts <- list(
    map_tiles = NULL,
    map_provider_tile = "leaflet",
    background_color = "#000"
  )
  leaflet() |>
    addTiles() |>
    lt_tiles(opts = opts)

  opts_data <- list(map_name = "col_departments")
  data_geo <- data_map_draw(data = NULL,
                            dic = NULL,
                            var_geo = NULL,
                            var_num = NULL,
                            opts = opts_data)

  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ name,
                         color =  "#DDDDDD",
                         fillColor = "#FCFCFC",
                         fillOpacity = 1,
                         opacity = 1) |>
    lt_tiles(opts = opts)


  # Mapa con tile leaflet

  opts_add <- list(
    map_tiles = "JusticeMap.white"
  )
  opts <- modifyList(opts, opts_add)
  leaflet() |>
    addTiles() |>
    lt_tiles(opts = opts)


  opts_add <- list(
    map_tiles = "CartoDB.VoyagerOnlyLabels"
  )
  opts <- modifyList(opts, opts_add)
  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ name,
                         color =  "#DDDDDD",
                         fillColor = "#FCFCFC",
                         fillOpacity = 0.5,
                         opacity = 1) |>
    lt_tiles(opts = opts)

  # Map with esri tile
  opts_add <- list(
    map_tiles = NULL,
    map_provider_tile = "esri",
    map_tiles_esri = "NationalGeographic"
  )
  opts <- modifyList(opts, opts_add)
  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ name,
                         color =  "#DDDDDD",
                         fillColor = "transparent",
                         fillOpacity = 0.5,
                         opacity = 1) |>
    lt_tiles(opts = opts)


  # Map with url tile (this must public tile)

  opts_add <- list(
    map_provider_tile = "url",
    map_extra_layout = "https://maps.geoapify.com/v1/tile/positron/{z}/{x}/{y}.png?&apiKey=f39345000acd4188aae1f2f4eed3ff14"
  )

  opts <- modifyList(opts, opts_add)
  leaflet::leaflet(data_geo$map_data) |>
    leaflet::addPolygons(weight = 1,
                         label = ~ name,
                         color =  "#DDDDDD",
                         fillColor = "transparent",
                         fillOpacity = 0.5,
                         opacity = 1) |>
    lt_tiles(opts = opts)


})
