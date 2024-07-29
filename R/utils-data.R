data_prep <- function(data = NULL,
                      dic = NULL,
                      var_geo = NULL,
                      var_num = NULL,
                      conmap = NULL,
                      map_name = NULL, ...) {


  dgeo <- NULL
  no_conmap <- is.null(conmap)
  conmap <- geotable::gt_con(conmap)

  sf <- geotable::gt_sf(map_name, con = conmap) |>
    geotable::rename_dotdot()

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
      filter(!is.na(value))
    dmatch <- geotable::gt_match(data,
                                 map_name,
                                 unique = TRUE,
                                 con = conmap) |>
      select(name, value, "..gt_id", ..labels)
    dgeo <- sf |> left_join(dmatch, by = "..gt_id")
  }


  if(no_conmap){
    geotable::gt_discon(conmap)
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


