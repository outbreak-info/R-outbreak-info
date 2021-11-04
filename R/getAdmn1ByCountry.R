#' @title Retrieve COVID-19 data for states/provinces in a country or countries
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all states/provinces in one or more countries.
#'
#'
#' @param countries vector or list of country name(s)
#'
#' @return dataframe
#'
#' @examples
#' getAdmn1ByCountry("India")
#'
#' @export

getAdmn1ByCountry <- function(countries, ...){
  locations <- searchLocations(countries, admin_level = 0)
  if (is.null(locations)){
    stop("No countries selected")
  }
  data <- getEpiData(country_name = locations, admin_level = 1, ...)
  return(data)
}
