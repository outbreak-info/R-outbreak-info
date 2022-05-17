#' @title Get ISO3 codes
#'
#' @description Get ISO3 codes for World Bank regions, countries, states/provinces, metropolitan areas, and/or counties.
#'
#' @param locations_to_search vector or list of location (World Bank region, country, state/province, metropolitan area, county) name(s)
#'
#' @import jsonlite
#'
#' @return a vector or list of ISO3 codes
#'
#' @examples
#' getISO3(c("San Diego", "Virginia", "India"))
#'
#' @export

getISO3 <- function(locations_to_search){
  locs_of_interest=c()
  locs_not_found=c()
  locations_to_search <- tolower(locations_to_search)
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("(name.lower:\"", paste(i, collapse="\" OR name.lower:\""), "\")")
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=",location.ids," AND mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      dataurl <- URLencode(dataurl)
      tryCatch({
        resp <- GET(dataurl)
        if(resp$status_code == 400){
          resp <- fromJSON(content(resp, "text"), flatten=TRUE)
          success <- resp$success
        } else if (resp$status_code == 200) {
          resp <- fromJSON(content(resp, "text"), flatten=TRUE)
          scroll.id <- ifelse(is.null(resp$'_scroll_id'), scroll.id, resp$'_scroll_id')
          success <- resp$success
          hits <- data.frame()
          if(!is.null(resp$hits)) {
            if(class(resp$hits) == "data.frame"){
              results[[length(results) + 1]] <- resp$hits
              hits <- rbind_pages(results)
            }
            if (nrow(hits)==1){
              locs_of_interest=c(locs_of_interest, hits$location_id)
            }else{
              locs_not_found=c(locs_not_found, i)
            }
          }
        } else {
          stop("Could not connect to API. Check internet connection and try again.")
        }
      }, error = function(cond){
        stop(cond)
      }, warning = function(cibd){
        stop(cond)
      })
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
      location.ids <- paste0("(name.lower:", paste(locations[i], collapse=" OR name.lower:"), ")")
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(api.url, "query?q=",location.ids," AND mostRecent:true&fields=name,location_id,state_name,admin_level&fetch_all=true")
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        dataurl <- URLencode(dataurl)
        tryCatch({
          resp <- GET(dataurl)
          if(resp$status_code == 400){
            resp <- fromJSON(content(resp, "text"), flatten=TRUE)
            success <- resp$success
          } else if (resp$status_code == 200) {
            resp <- fromJSON(dataurl, flatten=TRUE)
            scroll.id <- resp$'_scroll_id'
            if(!is.null(resp$hits)){
              if(class(resp$hits) == "data.frame"){
                results[[length(results) + 1]] <- resp$hits
              }
            }
            success <- resp$success
          }
        })
      }
      if(length(results) == 0) {
        message(locs_not_found[i], " not found. Please check spelling.")
        next
      } else{
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
        cat(paste0(i, "\n"))
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$location_id[df$fullname==i])
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          cat("Expected input is Y or N\n")
          cat(paste0(i, "\n"))
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
