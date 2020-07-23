#' @title Retrieve COVID-19 data for states/provinces in a country or countries
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all states/provinces in one or more countries.
#'
#' @param countries: vector or list of country name(s)
#'
#' @return dataframe
#'
#' @examples
#' getAdmn1ByCountry("India")
#'
#' @export
#' @import jsonlite

getAdmn1ByCountry <- function(countries){
  locations <- searchLocations(countries, admin_level=0)
  scroll.id <- NULL
  results <- list()
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=country_name:(",location.ids,")&fetch_all=true&sort=-date&admin_level=1")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits)
}
