#' @title Retrieve sequence count data
#'
#' @description Retrieves number of sequences by location, by date of sample collection. 
#' See \link[outbreakinfo]{getSubmissionDateByLocation} to view similar information by date of sequence submission.
#'
#'@param location (optional) a location name (if not specified, global total counts returned)
#'@param cumulative (optional) Boolean (T/F), T returns cumulative number of sequences to date
#'@param subadmin (optional) Boolean (T/F), subadmin=T and cumulative=T returns cumulative number of sequences for next administrative level
#'
#'@return dataframe
#'
#'@examples
#' # Retrieves the number of samples sequenced in the U.S. each day, by date of sample collection
#'getSeqCounts(location="United States")
#'
#' # Returns the total number of global sequences in the outbreak.info API.
#'getSeqCounts(cumulative=TRUE)
#'
#' @export


getSeqCounts <- function(location=NULL, cumulative=NULL, subadmin=NULL){
  df <- getGenomicData(query_url="sequence-count", location = location, cumulative = cumulative, subadmin = subadmin)
  return(df)
}
