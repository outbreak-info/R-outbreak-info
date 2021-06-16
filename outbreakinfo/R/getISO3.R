#' @title Get ISO3 codes
#'
#' @description Get ISO3 codes for World Bank regions, countries, states/provinces, metropolitan areas, and/or counties.
#'
#' @param locations_to_search: vector or list of location (World Bank region, country, state/province, metropolitan area, county) name(s)
#'
#' @return a vector or list of ISO3 codes
#'
#' @examples
#' getISO3(c("San Diego", "Virginia", "India"))
#'
#' @export
#' @import jsonlite

getISO3 <- function(locations_to_search){
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("(name:%22", paste(i, collapse="%22%20OR%20name:%22"), "%22)")
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
      if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
        stop("Could not connect to API. Check internet connection and try again.")
      }else{
        resp <- fromJSON(dataurl, flatten=TRUE)
        scroll.id <- resp$'_scroll_id'
        results[[length(results) + 1]] <- resp$hits
        success <- resp$success
      }
    }
    t <- try(rbind_pages(results), silent=T)
    if("try-error" %in% class(t)){
      error=T
      locs_not_found = c(locs_not_found, i)
    }else{
      hits <- rbind_pages(results)
      df=(hits)
      if (nrow(df)==1){
        locs_of_interest=c(locs_of_interest, df$location_id)
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
        dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name,admin_level&fetch_all=true")
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
        if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
          stop("Could not connect to API. Check internet connection and try again.")
        }else{
          resp <- fromJSON(dataurl, flatten=TRUE)
          scroll.id <- resp$'_scroll_id'
          results[[length(results) + 1]] <- resp$hits
          success <- resp$success
        }
      }
      t2 <- try(rbind_pages(results), silent=T)
      if("try-error" %in% class(t2)){
        print(paste(locs_not_found[i], "not found. Please check spelling."))
        next
      }else{
        hits <- rbind_pages(results)
        df=(hits)
        df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
        df$admin_level[df$admin_level == "-1"] <- "World Bank Region"
        df$admin_level[df$admin_level == "0"] <- "country"
        df$admin_level[df$admin_level == "1"] <- "state/province"
        df$admin_level[df$admin_level == "1.5"] <- "metropolitan area"
        df$admin_level[df$admin_level == "2"] <- "county"
        df$fullname <- paste0(df$name, " (", df$admin_level, ")")
      }
      for (i in df$fullname){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$location_id[df$fullname==i])
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, df$location_id[df$fullname==i])
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}
