data_prep <- function(data = NULL,
                      dic = NULL,
                      var_geo = NULL,
                      var_num = NULL,
                      conmap = NULL,
                      map_name = NULL,
                      map_file = NULL, ...) {


  dgeo <- NULL
  no_conmap <- is.null(conmap)
  conmap <- geotable::gt_con(conmap)
  if (is.null(map_name)) {
   sf <- sf::read_sf(map_file) |> select(id, name, geom) |>
     geotable::rename_dotdot()
  } else {
  sf <- geotable::gt_sf(map_name, con = conmap) |>
    geotable::rename_dotdot()
  }

  if (is.null(data)) {
    dgeo <- sf |> mutate(..labels = ..gt_name)
    dgeo$value <- 0
  } else {
    dic <- dic %||% hdtable(data)$dic
    var_geo <- var_geo %||% default_var_group(dic)

    data <- aggregate_data(data = data,
                           dic = dic,
                           group_vars = var_geo,
                           var_num_to_agg = var_num, ...)

    if (is.null(var_num)) var_num <- "Conteo"
    data[[var_geo]] <- toupper(data[[var_geo]])
    data <- data |>
      rename(name = {{var_geo}}, value = {{var_num}}) |>
      filter(!is.na(value), !is.na(name))
    if (is.null(map_name)) {
      sf$name <- toupper(sf$..gt_name)
      dgeo <- sf |> left_join(data)
    } else {
    dmatch <- geotable::gt_match(data,
                                 map_name,
                                 unique = TRUE,
                                 con = conmap) |>
      select(name, value, "..gt_id", ..labels)
    dgeo <- sf |> left_join(dmatch, by = "..gt_id")
    }
  }


  if(no_conmap){
    geotable::gt_discon(conmap)
  }
  print(dgeo)
  dgeo


}


data_prep_bubbles <- function(data = NULL,
                              dic = NULL,
                              var_cat = NULL,
                              var_geo = NULL,
                              var_gln = NULL,
                              var_glt = NULL,
                              var_num = NULL,
                              conmap = NULL,
                              map_name = NULL, ...) {
  dgeo <- NULL
  if (!is.null(var_geo)) {
    no_conmap <- is.null(conmap)
    conmap <- geotable::gt_con(conmap)

    sf <- geotable::gt_sf(map_name, con = conmap) |>
      geotable::rename_dotdot()
    dgeo <- data_prep(data, dic,
                      var_geo = var_geo, var_num = var_num,
                      conmap = conmap, map_name = map_name, ...)
    dgeo$centroides <- st_centroid(dgeo$geom)
    coords <- st_coordinates(dgeo$centroides)
    dgeo$lon <- coords[,"X"]
    dgeo$lat <- coords[,"Y"]
  } else {
    data <- data |> rename("lat" = {{var_gln}}, "lon" = {{var_glt}})
    opts <- dsopts_merge(..., categories = "dataprep")
    if (is.null(var_num)) {
      agg_num <- "count"
    } else {
      agg_num <- opts$agg %||% "sum"
    }
    data_viz <- dsdatawiz:::aggregate(
      data = data,
      group_vars = c(var_cat, "lat", "lon"),
      var_num_to_agg = var_num,
      agg = agg_num, agg_na_rm = TRUE,
    )


    if (!is.null(dic)) {
      dic$id[dic$id == var_gln] <- "lat"
      dic$id[dic$id == var_glt] <- "lon"

      if (is.null(var_num)) {
        var_num <- "conteo"
        dic <- bind_rows(dic,
                         data.frame(id = "conteo", label = "Conteo", hdtype = "Num"))
      }
    }

    var_num <- var_num %||% "conteo"
    ht <- hdtable(data_viz)
    data <- ht$data
    dic_d <- dic %||% ht$dic
    if (is.null(dic)) {
      dic_d$label[dic_d$id == "lat"] <- var_gln
      dic_d$label[dic_d$id == "lon"] <- var_glt
    }

    dgeo <- aggregate_data(data = data,
                           dic = dic_d,
                           group_vars = NULL,
                           var_num_to_agg = var_num, ...)
    var_value <- var_num
    if (!is.null(var_cat)) var_value <- var_cat
    dgeo$..domain <- dgeo[[var_num]]
    dgeo <- dgeo |>
      rename(value = {{var_value}})
  }


  dgeo
}



default_var_group <- function(dic = NULL) {
  available_hdtypes <- c("Gnm", "Gcd", "Cat")

  var_groups <- lapply(available_hdtypes, function(htype) {
    dsdatawiz:::guess_vars(dic, htype)[[paste0("var_", tolower(htype))]]
  })

  var_group <- Reduce(function(x, y) if (length(x) > 0) x else y, var_groups, init = NULL)

  var_group
}

default_var_coor <- function(dic = NULL) {
  dic <- dic |> filter(hdtype %in% "Num")
  if (nrow(dic) == 0) return()

}
