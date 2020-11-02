#' @title Retrieve COVID-19 data
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for one or more World Bank regions, countries, states/provinces, metropolitan areas, and/or counties.
#'
#' @param location_names: vector or list of location (World Bank region, country, state/province, metropolitan area, county) name(s)
#'
#' @return dataframe
#'
#' @examples
#' getLocationData(c("Texas", "Brazil", "San Diego County"))
#'
#' @export
#' @import jsonlite

getLocationData <- function(location_names){
  locations <- getISO3(location_names)
  scroll.id <- NULL
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=location_id:(",location.ids,")&sort=date&size=1000&fetch_all=true")
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits);
}
