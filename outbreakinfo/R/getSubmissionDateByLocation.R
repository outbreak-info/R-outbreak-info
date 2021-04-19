#' @title Retrieve most recent submission date by location
#'
#' @description Retrieve most recent submission date by location
#'
#'
#' @export
#' @import jsonlite

getSubmissionDateByLocation <- function(pangolin_lineage, location, mutations){
  df <- getGenomicData(query_url="most-recent-submission-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}
