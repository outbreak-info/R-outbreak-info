.onLoad <- function(libname, pkgname){
  api.url <- "https://api.outbreak.info/covid19/"
  assign("api.url", api.url, envir = parent.env(environment()))
  assign("COLORPALETTE", c("#bab0ab", "#4E79A7", "#aecBe8", "#f28e2b", "#FFBE7D", "#59a14f", "#8CD17D", "#e15759", "#FF9D9A", "#499894", "#86BCB6", "#B6992D", "#F1CE63", "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6", "#bcbd22", "#79706E"), envir = parent.env(environment()))
}
