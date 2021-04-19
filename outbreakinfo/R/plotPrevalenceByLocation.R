#' @title Plot daily prevalence of a lineage by location
#'
#' @description Plots the daily prevalence of a PANGO lineage by location
#'
#'
#' @export
#' @import jsonlite
#' @import ggplot2



plotPrevalenceByLocation <- function(pangolin_lineage, location, mutations, cumulative){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  p <- ggplot(data=df, aes(x=date, y=proportion)) + geom_line()
  p <- p + geom_ribbon(aes(ymin=proportion_ci_lower, ymax=proportion_ci_upper), alpha=0.2)
  return(p)
}
