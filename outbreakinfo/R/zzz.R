.onLoad <- function(libname, pkgname){
  api.url <- "https://api.outbreak.info/covid19/"
  assign("api.url", api.url, envir = parent.env(environment()))
}
