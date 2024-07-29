lt_add_layers <- function(lt, map_name_layers) {

if (!is.null(map_name_layers)) {
  for (mp in map_name_layers) {
    topoData <- geojsonio::topojson_json(
      geotable::gt_sf(mp, con = NULL) |>
        geotable::rename_dotdot()
    )

    lt <- lt |>
      addTopoJSON(
        topojson = topoData,
        weight = 1.3,
        opacity = 1,
        fill = FALSE,
        color = "red"
      )
  }
}
  lt
}
