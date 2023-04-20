

lt_palette <- function(dgeo, opts){


  if(is.numeric(dgeo$..var)){
    palette <- opts$theme$palette_colors_sequential
  }else{
    palette <- opts$theme$palette_colors_categorical
  }
  domain <- unique(dgeo$..var)
  domain <- domain[!is.na(domain)]
  na.color <- opts$theme$na_color

  if(is.numeric(dgeo$..var)){
    # pal <- leaflet::colorNumeric(palette, domain = dgeo$..var,
    #                              na.color = opts$theme$na_color)
    pal <- "colorNumeric"

  }else{
    # pal <- leaflet::colorFactor(palette, domain = dgeo$..var,
    #                             na.color = opts$theme$na_color)
    pal <- "colorFactor"
  }

  # Add leaflet namespace
  pal <- paste0("leaflet::",pal)

  do.call(getfun(pal),
          list(
            palette = palette[1:length(domain)],
            domain = domain,
            na.color = na.color
          ))
}


