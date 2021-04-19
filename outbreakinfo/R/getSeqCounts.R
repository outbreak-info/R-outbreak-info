#' @title Retrieve sequence count data
#'
#' @description Retrieves number of sequences per day by location from outbreak.info
#'
#'
#' @export
#' @import jsonlite


getSeqCounts <- function(location, cumulative, subadmin){
  df <- getGenomicData(query_url="sequence-count", location = location, cumulative = cumulative, subadmin = subadmin)
  return(df)
}
