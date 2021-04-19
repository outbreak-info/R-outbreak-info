#' @title Retrieve most recent collection date by location
#'
#' @description Retrieve most recent collection date by location
#'
#'
#' @export
#' @import jsonlite

getCollectionDateByLocation <- function(pangolin_lineage, location, mutations){
  df <- getGenomicData(query_url="most-recent-collection-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}
