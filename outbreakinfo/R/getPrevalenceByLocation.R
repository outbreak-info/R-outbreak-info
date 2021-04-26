#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage by location
#'
#'
#' @export
#' @import jsonlite



getPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  return(df)
}
