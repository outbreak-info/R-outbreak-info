#' @title Retrieve mutations by lineage
#'
#' @description Retrieve mutations in a specified lineage above a threshold
#'
#'@param pangolin_lineage: PANGO lineage name
#'@param frequency: a number between 0 and 1 specifying the frequency threshold above which to return mutations (default=0.8)
#'
#' @return dataframe
#'
#' @examples
#' getMutByLineage(pangolin_lineage="P.1", frequency=0.8)
#'
#' @export
#' @import jsonlite


getMutByLineage <- function(pangolin_lineage, frequency=0.8){
  df <- getGenomicData(query_url="lineage-mutations", pangolin_lineage = pangolin_lineage, frequency = frequency)
  return(df)
}
