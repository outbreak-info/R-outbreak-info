#' @title Retrieve mutation details
#'
#' @description Retrieve details of specified mutation(s)
#'
#'
#' @export
#' @import jsonlite

getMutDetails <- function(mutations){
  df <- getGenomicData(query_url="mutation-details", mutations = mutations)
  return(df)
}
