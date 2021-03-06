#' @title Retrieve COVID-19 data for given locations
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for specified location(s)
#'
#' @param location_names: vector or list of location name(s)
#'
#' @return dataframe
#'
#' @examples
#' getLocationData("California", "India")
#'
#' @export
#' @import jsonlite
#' @import progress


getLocationData <- function(location_names, ...){
  location_codes <- getISO3(location_names)
  data <- getEpiData(location_id=location_codes, ...)
  return(data)
}
