test_that("Bubble map", {
  lt_bubbles()
  lt_bubbles(map_name = "col_departments")

  data <- data.frame(
    Latitud = runif(30, min = -90, max = 90),
    Longitud = runif(30, min = -180, max = 180)
  )

  lt_bubbles(data = data, var_gln = "Longitud", var_glt = "Latitud")

  # Añadir agrupación
  lt_bubbles(
    data = data,
    var_gln = "Longitud",
    var_glt = "Latitud",
    map_bubble_cluster = TRUE
  )

  # Personalizar agrupación
  # aca se puede ajustar:
  # showCoverageOnHover: Cuando se pasa el cursor sobre un grupo de marcadores, muestra la cobertura del área que representa ese grupo.
  # zoomToBoundsOnClick: Al hacer clic en un grupo de marcadores, el mapa se acercará para mostrar todos los marcadores individuales de ese grupo
  # spiderfyOnMaxZoom: Si el usuario hace clic en un grupo de marcadores cuando el mapa está en su nivel de zoom máximo, los marcadores se "desplegarán" (spiderfy) para mostrarlos individualmente.
  # removeOutsideVisibleBounds: Elimina los grupos de marcadores que están fuera de los límites visibles del mapa para mejorar el rendimiento.
  # animateAddingMarkers: Controla si los nuevos marcadores se añaden con una animación.
  # disableClusteringAtZoom: Desactiva la agrupación de marcadores a un nivel de zoom especificado.
  # maxClusterRadius: Define el radio máximo en píxeles de un grupo de marcadores. Los marcadores que están más lejos no se agruparán
  # spiderLegPolylineOptions: markerClusterOptions(spiderLegPolylineOptions = list(weight = 1.5, color = "#FF0000"))
  # iconCreateFunction: Una función JavaScript que permite personalizar la apariencia del ícono del grupo de marcadores.
  icon_create_function <- JS(
    "
      function(cluster) {
        return L.divIcon({
          html: '<div style=\"display: flex; justify-content: center; align-items: center; width: 40px; height: 40px; border-radius: 50%; background: rgba(255, 0, 0, 0.5);\"><span>' + cluster.getChildCount() + '</span></div>',
          className: 'marker-cluster',
          iconSize: L.point(40, 40)
        });
      }
    "
  )

  lt_bubbles(
    data = data,
    var_gln = "Longitud",
    var_glt = "Latitud",
    map_bubble_cluster = TRUE,
    map_bubble_cluster_params = list(iconCreateFunction = icon_create_function)
  )

  data <- data.frame(
    lng = c(-99.13, -99.14, -99.15, -99.16),
    lat = c(19.43, 19.44, 19.45, 19.46),
    category = c("A", "B", "A", "C")
  )

  lt_bubbles(
    data = data,
    var_gln = "lat",
    var_glt = "lng",
    var_cat = "category",
    map_name = "world_countries_mexico"
  )
})

test_that("lt_bubbles_Geo", {
  data <- data.frame(
    name = c(
      rep("BOGOTA, D.C.", 5),
      rep("CAUCA", 3),
      rep("ANTIOQUIA", 2),
      rep("MAGDALENA", 1)
    )
  )

  lt_bubbles_Geo(data, map_name = "col_departments")

  # Color by intervals
  lt_bubbles_Geo(
    data,
    map_name = "col_departments",
    color_palette_type = "categorical"
  )
})

test_that("lt_bubbles_GeoNum", {
  data <- data.frame(
    name = c("BOGOTA, D.C.", "CAUCA", "ANTIOQUIA", "MAGDALENA", "MAGDALENA"),
    population = c(8000000, 1500000, 6500000, 1300000, 32423)
  )

  lt_bubbles_GeoNum(data, map_name = "col_departments")
  lt_bubbles_GeoNum(data, map_name = "col_departments", agg = "mean")

  # Color by intervals
  lt_bubbles_GeoNum(
    data,
    map_name = "col_departments",
    color_palette_type = "categorical"
  )

  lt_bubbles_GeoNum(
    data,
    map_name = "col_departments",
    color_palette_type = "categorical",
    color_bins_n = 3
  )

  lt_bubbles_GeoNum(
    data,
    map_name = "col_departments",
    color_palette_type = "categorical",
    color_bins_n = 10
  )
})

test_that("lt_bubbles_GeoCat", {
  data <- data.frame(
    name = c(
      "BOGOTA, D.C.",
      "CAUCA",
      "ANTIOQUIA",
      "MAGDALENA",
      "SANTANDER",
      "NARIÑO",
      "VALLE DEL CAUCA",
      "META"
    ),
    category = c(
      "A",
      "B",
      "C",
      "D",
      "B",
      "C",
      "A",
      "D"
    )
  )

  lt_bubbles_GeoCat(data, map_name = "col_departments")
})
