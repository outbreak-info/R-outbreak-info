#' @title Plot COVID-19 data of interest
#'
#' @description Plot a metric of interest using up-to-date COVID-19 data from outbreak.info for location(s) of interest (World Bank region, country, state/province, metropolitan area, county).
#'
#'@param locations: a vector or list of location names
#'@param variable: metric to plot
#'
#' @return ggplot2 object
#'
#' @examples
#' p=plotCovid("Florida", "confirmed_per_100k")
#' show(p)
#'
#' @export
#' @import ggplot2
#' @import progress


plotCovid <- function(locations, variable){
  if (missing(variable)){
    stop("Variable to plot not specified")
  }
  location_codes <- getISO3(locations)
  df <- getEpiData(location_id=location_codes)
  if (!(variable %in% colnames(df))){
    print(paste(variable, "is not a valid API field"))
    return(NULL)
  }
  p=ggplot(df, aes(date, get(variable), color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 month") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y=variable)
  return(p)
}

