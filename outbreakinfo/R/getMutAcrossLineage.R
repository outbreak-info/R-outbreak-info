#' @title Retrieve prevalence of mutation across lineages by location
#'
#' @description Retrieve prevalence of specified mutation across all lineages by location
#'
#'
#' @export
#' @import jsonlite


getMutAcrossLineage <- function(mutations, location){
  df <- getGenomicData(query_url="mutations-by-lineage", mutations = mutations, location = location)
  return(df)
}
