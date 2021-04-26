#' @title Retrieve lag between collection and submission
#'
#' @description Retrieve daily lag between collection and submission by location
#'
#'@param location: (optional) a location name (if not specified, daily lag globally returned)
#'
#'@return dataframe
#'
#'@examples
#'getLag(location="California")
#'
#' @export
#' @import jsonlite

getLag <- function(location=NULL){
  df <- getGenomicData(query_url="collection-submission", location = location)
  return(df)
}
