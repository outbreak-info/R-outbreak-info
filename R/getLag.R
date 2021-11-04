#' @title Retrieve lag between collection and submission
#'
#' @description Retrieve daily lag between collection and submission by location
#'
#'@param location (optional) a location name (if not specified, daily lag globally returned)
#'
#'@return dataframe
#'
#'@examples
#'getLag(location="California")
#'
#' @export

getLag <- function(location=NULL){
  df <- getGenomicData(query_url="collection-submission", location = location)
  if(!is.null(df) && nrow(df) > 0) {
    df$date_collected <- as.Date(df$date_collected, "%Y-%m-%d")
    df$date_submitted <- as.Date(df$date_submitted, "%Y-%m-%d")
    df$lag <- df$date_submitted - df$date_collected
  }
  return(df)
}
