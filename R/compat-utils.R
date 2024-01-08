#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr select mutate summarise
#' @importFrom dstools %||%
## usethis namespace: end
NULL

#' @keywords internal
getfun <- function(x) {
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

#' @keywords internal
frtype_viz <- function(var_gnm = NULL,
                       var_gcd = NULL,
                       var_gln = NULL,
                       var_glt = NULL,
                       var_num = NULL) {

  frtype <- NULL
  if (!is.null(var_gnm)) frtype <- paste0(rep("Gnm", length(var_gnm)), collapse = "")
  if (!is.null(var_gcd)) frtype <- paste0(frtype, paste0(rep("Gcd", length(var_gcd)), collapse = ""), collapse = "")
  if (!is.null(var_gln)) frtype <- paste0(frtype, paste0(rep("Gln", length(var_gln)), collapse = ""), collapse = "")
  if (!is.null(var_glt)) frtype <- paste0(frtype, paste0(rep("Glt", length(var_glt)), collapse = ""), collapse = "")
  if (!is.null(var_num)) frtype <- paste0(frtype, paste0(rep("Num", length(var_num)), collapse = ""), collapse = "")

  frtype
}

#' @keywords internal
data_map_draw <- function(data = NULL,
                          dic = NULL,
                          var_geo = NULL,
                          var_num = NULL,
                          opts = NULL) {

  map_name <- opts$map_name
  if(is.null(map_name)) stop("No map name provided, see available_maps()")

  tj <- geodato::gd_tj(map_name)

  if(!is.null(data)){

    vars <- c(var_geo, var_num)
    data <- data |>
      select(vars, everything())

    if (!"..labels" %in% names(data)) {
      data$label <- dsdataprep::prep_tooltip(data = data,
                                             tooltip = opts$tooltip_template,
                                             new_labels = NULL,
                                             engine = "html",
                                             as_df = FALSE,
                                             na_row_default_column = NULL,
                                             na_row_default_value = NULL,
                                             na_label = opts$na_label,
                                             format_num = opts$format_sample_num,
                                             opts_format_num = list(prefix = opts$prefix_num,
                                                                    suffix = opts$suffix_num,
                                                                    si_prefix = opts$si_prefix),
                                             format_cat = opts$format_sample_cat,
                                             format_date = opts$format_sample_dat)
    } else {
      data <- data |> dplyr::rename(label = ..labels)
    }

    if (!is.null(var_num)) {
      data$..domain <- data[[var_num]]
    }

    if (length(var_geo) == 1) {

      dgeo <- tryCatch({
        data_join <- geodato::gd_match(data, map_name)
        tj |>
          dplyr::left_join(data_join, by = c(id = "..gd_id", name = "..gd_name"))
      },
      error = function(e) {
        data_join <- data |> dplyr::rename(..gd_name = !!var_geo)
        data_join$..gd_name <- stringi::stri_trans_general(tolower(data_join$..gd_name), "Latin-ASCII")
        tj$..gd_name <- stringi::stri_trans_general(tolower(tj$name), "Latin-ASCII")
        tj |>
          dplyr::left_join(data_join)
      })

      if ("label" %in% names(dgeo)) {
        dgeo <- dgeo |>
          mutate(label = ifelse(is.na(label), name, label) |>
                   lapply(htmltools::HTML))
      }
    } else {
      dgeo <- tj
      dgeo$label <- dgeo$name
      dgeo$..domain <- NA
      if ("label" %in% names(data)) {
        data$label <- lapply(data$label, htmltools::HTML)
      }
      if (opts$data_rename) {
        var_lon <- var_geo[1]
        var_lat <- var_geo[2]
        data <- data |> dplyr::rename(lon = {{var_lon}},
                                      lat = {{var_lat}})
      }
      dgeo <- list(dgeo = dgeo,
                   data = data)
    }

  } else {
    dgeo <- tj
    dgeo$label <- dgeo$name
    dgeo$..domain <- NA
  }

  if(!is.null(opts$filter)){
    code_or_name <- is_code_or_name(opts$filter, map_name)
    if(opts$code_or_name == "name"){
      filter <- tibble::tibble(filter = opts$filter) |>
        gd_match(map_name) |> dplyr::pull(..gd_id)
    }
    dgeo <- dgeo |> dplyr::filter(id %in% filter)
  }

  list(
    var_geo = var_geo,
    map_data = dgeo
  )


}
