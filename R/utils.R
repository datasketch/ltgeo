
getfun <- function(x) {
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

tmp_fun <- function(){
  cars |>
    mutate(..val = speed)

}


