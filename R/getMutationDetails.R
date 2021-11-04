#' @title Retrieve mutation details
#'
#' @description Retrieve details of specified mutation(s)
#'
#'@param mutations a `vector` of mutation(s)
#'
#'@return dataframe
#'
#'@examples
#'getMutationDetails(mutations=c("S:E484K", "S:N501Y"))
#'
#' @export

getMutationDetails <- function(mutations){
  df <- getGenomicData(query_url="mutation-details", mutations = mutations)
  return(df)
}
