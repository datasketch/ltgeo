

get_leaflet_option <- function(l, option = NULL){

  if(!inherits(l, "leaflet"))
    stop("Must be a leaflet object")

  calls <- l$x$calls

  lt_opts <- list()

  ### Here lives the logo information
  #l[["jsHooks"]][["render"]][[1]][["code"]]


  opts <- purrr::map(calls, function(m){
    # if(m$method == "setMapWidgetStyle"){
    #   opts <- m$args[[1]]
    #   str(lt_opts)
    # }
    opts <- m$args[[1]]
    opts
  })
  opts <- purrr::list_flatten(opts)
  opts <-dstools::removeNulls(opts)
  if(is.null(option)) return(opts)
  opts[[option]]

}

