#' @title Plot prevalence of lineages by location
#'
#' @description Plot prevalence of all lineages above a specified frequency over time by location
#'
#'@param location: a location name
#'@param other_threshold: minimum prevalence threshold below which lineages are accumulated under "Other" (default=0.05)
#'@param nday_threshold: minimum number of days in which the prevalence of a lineage must be below other_threshold to be accumulated under "Other" (default=10)
#'@param ndays: the number of days before the current date to be used as a window to accumulate lineages under "Other" (default=180)
#'@param other_exclude: (optional) lineage(s) that are NOT to be included under "Other" even if the conditions specified by the three thresholds above are met
#'@param cumulative: `Boolean` (T/F), T returns cumulative prevalence of lineages (default=F)
#'@param include_title: `Boolean` (T/F), T returns plot with title, F returns plot without title (default=F)
#'
#'@return ggplot2 object
#'
#'@examples
#'plotAllLineagesByLocation(location = "India", other_threshold=0.03, nday_threshold=60)
#'
#' @export
#' @import jsonlite
#' @import ggplot2
#' @import stringr

plotAllLineagesByLocation <- function(location, other_threshold=0.05, nday_threshold=10, ndays=180, other_exclude=NULL, cumulative=F, include_title = F){
  df <- getGenomicData(query_url="prevalence-by-location-all-lineages", location = location, other_threshold = other_threshold, nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, cumulative = cumulative)
  cat("Plotting data...", "\n")
  p <- ggplot(df, aes(x=date, y=prevalence_rolling, group=lineage, fill=lineage)) + geom_area()
  if (include_title == T){
    p <- p + ggtitle(paste0("Lineage prevalence in ", str_to_title(location)))
  }
  return(p)
}
