
get_opts <- function(...){
  if(length(list(...)) == 0){
    return(dsvizopts::merge_dsviz_options())
  }
  dsvizopts::merge_dsviz_options(...)
}

