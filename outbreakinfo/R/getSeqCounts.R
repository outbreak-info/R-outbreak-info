#' @title Retrieve sequence count data
#'
#' @description Retrieves number of sequences per day by location
#'
#'@param location: (optional) a location name (if not specified, global total counts returned)
#'@param cumulative: (optional) Boolean (T/F), T returns cumulative number of sequences to date
#'@param subadmin: (optional) Boolean (T/F), subadmin=T and cumulative=T returns cumulative number of sequences for next administrative level
#'
#'@return dataframe
#'
#'@examples
#'getSeqCounts(location="United States of America")
#'
#' @export


getSeqCounts <- function(location=NULL, cumulative=NULL, subadmin=NULL){
  df <- getGenomicData(query_url="sequence-count", location = location, cumulative = cumulative, subadmin = subadmin)
  return(df)
}
