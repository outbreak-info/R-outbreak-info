#' @title Plot COVID-19 data of interest
#'
#' @description Plot a metric of interest using up-to-date COVID-19 data from outbreak.info for location(s) of interest (World Bank region, country, state/province, metropolitan area, county).
#'
#'@param locations: a vector or list of location names
#'@param key: metric to plot
#'
#' @return ggplot2 object
#'
#' @examples
#' p=plotCovid("Florida", "confirmed_per_100k")
#' show(p)
#'
#' @export
#' @import ggplot2


plotCovid <- function(locations, key){
  df <- getLocationData(locations)
  df$date=as.Date(df$date, "%Y-%m-%d")
  p=ggplot(df, aes(date, get(key), color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y=key)
  return(p)
}
