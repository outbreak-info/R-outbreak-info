.onLoad <- function(libname, pkgname){
  api.url <- "https://api.outbreak.info/covid19/"
  assign("api.url", api.url, envir = parent.env(environment()))
  assign("COLORPALETTE", c("#bab0ab", "#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                           "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E"), envir = parent.env(environment()))
}
