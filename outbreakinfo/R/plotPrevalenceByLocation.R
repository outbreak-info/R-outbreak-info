#' @title Plot daily prevalence of a lineage by location
#'
#' @description Plots the daily prevalence of a PANGO lineage by location
#'
#'@param pangolin_lineage: PANGO lineage name
#'@param location: a location name
#'@param mutations: (optional) a `vector` of mutation(s)
#'@param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#'@param include_title: `Boolean` (T/F), T returns plot with title, F returns plot without title (default=F)
#'
#'@return ggplot2 object
#'
#'@examples
#'plotPrevalenceByLocation(pangolin_lineage = "P.1", location = "Brazil")
#'
#'
#' @export
#' @import jsonlite
#' @import ggplot2


plotPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL, include_title=F){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  p <- ggplot(data=df, aes(x=date, y=proportion)) + geom_line() + scale_y_continuous(labels = scales::percent, name="percentage")
  p <- p + geom_ribbon(aes(ymin=proportion_ci_lower, ymax=proportion_ci_upper), alpha=0.2)
  if (include_title == T){
    p <- p + ggtitle(paste0("Prevalence of ", pangolin_lineage, " in ", location))
  }
  return(p)
}
