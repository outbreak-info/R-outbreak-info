#' @title Retrieve most recent submission date
#'
#' @description Retrieves number of sequences by location, by date of sequence submission. 
#' See \link[outbreakinfo]{getSeqCounts} to view similar information by date of sample collection.
#'
#'@param pangolin_lineage PANGO lineage name
#'@param location (optional) a location name (if not specified, submission date at the global level returned)
#'@param mutations (optional) a `vector` of mutation(s)
#'
#'@return dataframe
#'
#'@examples
#'getSubmissionDateByLocation(pangolin_lineage="B.1.1.7", location="California")
#'
#' @export

getSubmissionDateByLocation <- function(pangolin_lineage, location=NULL, mutations=NULL){
  df <- getGenomicData(query_url="most-recent-submission-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}
