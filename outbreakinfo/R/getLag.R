#' @title Retrieve lag between collection and submission by location
#'
#' @description Retrieve daily lag between collection and submission by location
#'
#'
#' @export
#' @import jsonlite

getLag <- function(location=NULL){
  df <- getGenomicData(query_url="collection-submission", location = location)
  return(df)
}
