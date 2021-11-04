#' @title Retrieve COVID-19 data for countries in a World Bank region(s)
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all countries in one or more World Bank region(s).
#'
#' @param wb_regions vector or list of World Bank region(s)
#'
#' @return dataframe
#'
#' @examples
#' getCountryByRegion("North America")
#'
#' @export

getCountryByRegion <- function(wb_regions, ...){
  locations <- searchLocations(wb_regions, admin_level = -1)
  if (is.null(locations)){
    stop("No regions selected")
  }
  data <- getEpiData(wb_region = locations, admin_level = 0, ...)
  return(data)
}
