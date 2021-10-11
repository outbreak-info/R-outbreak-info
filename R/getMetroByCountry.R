#' @title Retrieve COVID-19 data for all metropolitan areas in the US.
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all metropolitan areas in the United States of America.
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' getMetroByCountry()
#' }

getMetroByCountry <- function(...){
  data <- getEpiData(country_name = "United States of America", admin_level = 1.5, ...)
  return(data)
}
