#' @title Retrieve prevalence of lineages by location
#'
#' @description Retrieve prevalence of all lineages over time by location
#'
#'
#' @export
#' @import jsonlite


getAllLineagesByLoc <- function(location, other_threshold=0.05, nday_threshold=10, ndays=180, other_exclude, cumulative){
  df <- getGenomicData(query_url="prevalence-by-location-all-lineages", location = location, other_threshold = other_threshold, nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, cumulative = cumulative)
  return(df)
}
