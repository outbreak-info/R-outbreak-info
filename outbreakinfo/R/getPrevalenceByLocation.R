#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage by location
#'
#'@param pangolin_lineage: PANGO lineage name
#'@param location: a location name
#'@param mutations: (optional) a `vector` of mutation(s)
#'@param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#'
#'@return dataframe
#'
#'@examples
#'getPrevalenceByLocation(pangolin_lineage = "P.1", location = "Brazil")
#'
#' @export
#' @import jsonlite



getPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  return(df)
}
