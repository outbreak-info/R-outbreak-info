#' @title Get ISO3 codes from genomic API
#'
#' @description Get ISO3 codes for World Bank regions, countries, states/provinces, metropolitan areas, and/or counties from genomic API
#'
#' @param locations_to_search: a location name
#'
#' @return an ISO3 code
#'
#' @examples
#' getISO3_genomic("San Diego")
#'
#' @export
#' @import jsonlite
#' @import plyr


getISO3_genomic <- function(locations_to_search){
  loc_url <- "https://api.outbreak.info/genomics/location?"
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("name=", paste(i))
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(loc_url, location.ids)
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
      if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
        stop("Could not connect to API. Check internet connection and try again.")
      }else{
        resp <- fromJSON(dataurl, flatten=TRUE)
        results[[length(results) + 1]] <- resp$results
        scroll.id <- resp$'_scroll_id'
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
        locs_of_interest=c(locs_of_interest, df$id)
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
      location.ids <- paste0("name=", paste(locations[i]))
      location.ids <- gsub("&", "%26", location.ids)
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(loc_url, location.ids)
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
        if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
          stop("Could not connect to API. Check internet connection and try again.")
        }else{
          resp <- fromJSON(dataurl, flatten=TRUE)
          results[[length(results) + 1]] <- resp$results
          scroll.id <- resp$'_scroll_id'
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
        df$admin_level[df$admin_level == "-1"] <- "World Bank Region"
        df$admin_level[df$admin_level == "0"] <- "country"
        df$admin_level[df$admin_level == "1"] <- "state/province"
        df$admin_level[df$admin_level == "1.5"] <- "metropolitan area"
        df$admin_level[df$admin_level == "2"] <- "county"
        df$full <- paste0(df$label, " (", df$admin_level, ")")
      }
      for (i in df$full){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$id[df$full==i])
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, df$id[df$full==i])
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}
