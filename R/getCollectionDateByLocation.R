#' @title Retrieve most recent collection date
#'
#' @description Retrieve most recent collection date by location
#'
#'@param pangolin_lineage PANGO lineage name
#'@param location (optional) a location name (if not specified, collection date at the global level returned)
#'@param mutations (optional) a `vector` of mutation(s)
#'
#'@return dataframe
#'
#'@examples
#'getCollectionDateByLocation(pangolin_lineage="B.1.1.7", location="California")
#'
#' @export

getCollectionDateByLocation <- function(pangolin_lineage, location=NULL, mutations=NULL){
  df <- getGenomicData(query_url="most-recent-collection-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}
