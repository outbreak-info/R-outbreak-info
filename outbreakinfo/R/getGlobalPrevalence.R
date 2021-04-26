#' @title Retrieve global daily prevalence of a lineage
#'
#' @description Retrieves the global daily prevalence of a PANGO lineage
#'
#'
#' @export
#' @import jsonlite


getGlobalPrevalence <- function(pangolin_lineage, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="global-prevalence", pangolin_lineage = pangolin_lineage, mutations = mutations, cumulative = cumulative)
  return(df)
}
