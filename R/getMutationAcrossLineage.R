#' @title Retrieve prevalence of mutation across lineages by location
#'
#' @description Retrieve prevalence of a specified mutation across all lineages by location
#'
#'@param mutations a `vector` of mutation(s)
#'@param location (optional) a location name (if not specified, prevalence globally returned)
#'
#'@return dataframe
#'
#'@examples
#'getMutationAcrossLineage(mutations="S:N501Y", location="United States")
#'
#' @export


getMutationAcrossLineage <- function(mutations, location=NULL){
  df <- getGenomicData(query_url="mutations-by-lineage", mutations = mutations, location = location)
  return(df)
}
