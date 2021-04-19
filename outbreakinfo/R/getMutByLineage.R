#' @title Retrieve mutations by lineage
#'
#' @description Retrieve mutations in a specified lineage above an optional frequency threshold
#'
#'
#' @export
#' @import jsonlite


getMutByLineage <- function(pangolin_lineage, frequency=0.8){
  df <- getGenomicData(query_url="lineage-mutations", pangolin_lineage = pangolin_lineage, frequency = frequency)
  return(df)
}
