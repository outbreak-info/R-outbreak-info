#' @title Retrieve cumulative prevalence of a lineage
#'
#' @description Retrieve cumulative prevalence of a PANGO lineage by the immediate administrative level of a location
#'
#'
#' @export
#' @import jsonlite

getCumulativeBySubadmin <- function(pangolin_lineage, location, mutations, ndays){
  df <- getGenomicData(query_url="lineage-by-sub-admin-most-recent", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, ndays = ndays)
  return(df)
}
