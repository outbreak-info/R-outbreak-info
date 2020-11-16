#' @title Get exact location names
#'
#' @description Get exact spelling of locations at the same administrative level.
#'
#' @param locations_to_search: vector or list of location (World Bank region, country, state/province, metropolitan area, county) name(s) at the same administrative level
#' @param admin_level: an integer representing an administrative level (World Bank regions = -1, countries = 0, states/provinces = 1, metropolitan areas = 1.5, counties = 2)
#'
#' @return a vector or list of location names
#'
#' @examples
#' searchLocations(c("California", "Florida", "Texas"), admin_level=1)
#'
#' @export
#' @import jsonlite

searchLocations <- function(locations_to_search, admin_level){
  if (missing(admin_level)){
    stop("Administrative level not specified")
  }
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("(name:%22", paste(i, collapse="%22%20OR%20name:%22"), "%22)")
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=", location.ids, "%20AND%20", "admin_level:%22", admin_level, "%22", "%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
    }
    t <- try(rbind_pages(results), silent=T)
    if("try-error" %in% class(t)){
      error=T
      locs_not_found = c(locs_not_found, i)
    }else{
      hits <- rbind_pages(results)
      df=(hits)
      if (nrow(df)==1){
        locs_of_interest=c(locs_of_interest, i)
      }else{
        locs_not_found=c(locs_not_found, i)
      }
    }
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(locs_of_interest)
  }
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    for (i in locs_not_found){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    for (i in 1:length(locations)){
      scroll.id <- NULL
      location.ids <- paste0("(name:", paste(locations[i], collapse="%20OR%20name:"), ")")
      location.ids <- gsub("&", "%26", location.ids)
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(api.url, "query?q=", location.ids, "%20AND%20", "admin_level:%22", admin_level, "%22", "%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        resp <- fromJSON(dataurl, flatten=TRUE)
        scroll.id <- resp$'_scroll_id'
        results[[length(results) + 1]] <- resp$hits
        success <- resp$success
      }
      t2 <- try(rbind_pages(results), silent=T)
      if("try-error" %in% class(t2)){
        print(paste(locs_not_found[i], "not found. Please check spelling."))
        next
      }else{
        hits <- rbind_pages(results)
        df=(hits)
        df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
      }
      for (i in df$name){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, i)
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, i)
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}
