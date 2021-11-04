#' @title Retrieve COVID-19 data for all locations at an administrative level.
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all locations at a given administrative level.
#'
#' @param admin_level an integer representing an administrative level (World Bank regions = -1, countries = 0, states/provinces = 1, metropolitan areas = 1.5, counties = 2)
#'
#' @return dataframe
#'
#' @examples
#' getByAdmnLevel(-1)
#'
#' @export

getByAdmnLevel <- function(admin_level, ...){
  data <- getEpiData(admin_level = admin_level, ...)
  return(data)
}
