

prep_geo <- function(dd, map_name, var = NULL, opts){

  col <- geodato::parse_col(dd, var)
  dd$..var <- dd[[col]]
  if (is.character(dd$..var)) {
    dd$..var <- as.numeric(factor(dd$..var))
    var <- "..var"
  }

  # Merge geodata
  d <- geodato::gd_match(dd, map_name)
  tj <- geodato::gd_tj(map_name)

  if(!is.null(dd)){
    dgeo <- tj |>
      dplyr::left_join(d, by = c(id = "..gd_id", name = "..gd_name"))
  } else{
    dgeo <- d
  }

  # Guess type of column to color

  if (is.null(dgeo$..color)) {
    var_color <- opts$color_by %||% var
    v_color <- dd[[var_color]]

    palette_type <- paletero::which_palette_type(v_color)
    opts$color_palette_type <- opts$color_palette_type %||% palette_type
    palette <- opts[[paste0("color_palette_",opts$color_palette_type)]]
    dgeo$..color <- paletero::paletero(dgeo$..var,
                                       palette = palette,
                                       na_color = opts$na_color)
  }

  # Add tooltip
  # Calculate tooltip

  if(is.null(dgeo$..tooltip)){
    vars <- names(dgeo)[!grepl("^\\.\\.|geometry", names(dgeo))]
    tooltip <- opts$tooltip_template %||% NULL
    dd <- sf::st_drop_geometry(dgeo) |>
      dplyr::select(name, any_of(vars))
    dgeo$..tooltip <- dsdataprep::prep_tooltip(data = dd, tooltip = tooltip,
                                               na_row_default_column = "name")
    #tooltip <- dd$name
    dgeo$..labels <- purrr::map(dgeo$..tooltip, htmltools::HTML)
  }

  # Add popup
  if(opts$map_popup){
    # do something with popup?
    dgeo$..popup <- dgeo$..tooltip
    dgeo$..tooltip <- NULL
  }

  list(
    dgeo = dgeo,
    opts = opts
  )

}






