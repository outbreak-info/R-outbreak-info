#' @title Retrieve COVID-19 data for countries in a World Bank region(s)
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all countries in one or more World Bank region(s).
#'
#' @param wb_regions: vector or list of World Bank region(s)
#'
#' @return dataframe
#'
#' @examples
#' getCountryByRegion("North America")
#'
#' @export
#' @import jsonlite

getCountryByRegion <- function(wb_regions){
  locations <- searchLocations(wb_regions, admin_level=-1)
  scroll.id <- NULL
  results <- list()
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=wb_region:(",location.ids,")%20AND%20admin_level:0&fetch_all=true")
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
