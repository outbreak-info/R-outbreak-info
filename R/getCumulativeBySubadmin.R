#' @title Retrieve cumulative prevalence of a lineage
#'
#' @description Retrieve cumulative prevalence of a PANGO lineage by the immediate administrative level of a location
#'
#'@param pangolin_lineage PANGO lineage name
#'@param location (optional) a location name (if not specified, cumulative prevalence at the country level globally returned)
#'@param mutations (optional) a `vector` of mutation(s)
#'@param ndays (optional) an `integer` specifying number of days from current date to calculative cumuative counts (if not specified, no limit on window)
#'
#'@return dataframe
#'
#' @examples
#' # Worldwide prevalence of B.1.1.7 by country
#' head(getCumulativeBySubadmin(pangolin_lineage="B.1.1.7"))
#' 
#' # County-level prevalence of B.1.1.7
#' head(getCumulativeBySubadmin(pangolin_lineage="B.1.1.7", location="California"))
#'
#'
#' @export

getCumulativeBySubadmin <- function(pangolin_lineage, location=NULL, mutations=NULL, ndays=NULL){
  df <- getGenomicData(query_url="lineage-by-sub-admin-most-recent", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, ndays = ndays)
  
  
  if(!is.null(df) && nrow(df) != 0){
    df <- df %>%
      rename(lineage = query_key)
  }
  
  return(df)
}
