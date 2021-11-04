#' @title Retrieve global daily prevalence of a lineage
#'
#' @description Retrieves the global daily prevalence of a PANGO lineage
#'
#'@param pangolin_lineage PANGO lineage name
#'@param mutations (optional) a `vector` of mutation(s)
#'@param cumulative (optional) `Boolean` (T/F), T returns cumulative global prevalence since first day of detection
#'
#'@return dataframe
#'
#'@examples
#' # B.1.1.7 lineage
#'b117 = getGlobalPrevalence(pangolin_lineage = "B.1.1.7", mutations = "S:E484K")
#'head(b117)
#'
#' @export


getGlobalPrevalence <- function(pangolin_lineage, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="global-prevalence", pangolin_lineage = pangolin_lineage, mutations = mutations, cumulative = cumulative)
  return(df)
}
