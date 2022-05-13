#' @title Get location id codes from genomic API
#'
#' @description Get location ID code for countries, states/provinces, metropolitan areas, and/or counties from genomic API
#'
#' @param locations_to_search a location name
#'
#' @return a location ID code (ISO3, FIPS)
#'
#' @examples
#' getLocationIdGenomic("San Diego")
#'
#' @export

getLocationIdGenomic <- function(locations_to_search){
    loc_url <- "https://api.outbreak.info/genomics/location?"
    locs_of_interest=c()
    locs_not_found=c()
    for (i in locations_to_search){
        location.ids <- paste0("name=", paste(i))
        dataurl <- paste0(loc_url, location.ids)
        results <- getGenomicsResponse(dataurl, logInfo = F, logWarning = T, logError = T)
        hits <- data.frame()
        if(length(results) >= 1)
            hits <- rbind_pages(results)
        if(nrow(hits) == 0){
            locs_not_found = c(locs_not_found, i)
        } else {
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
            }else{
                locs=paste0("*",i,"*")
            }
            locations=c(locations, locs)
        }
        for (i in 1:length(locations)){
            location.ids <- paste0("name=", paste(locations[i]))
            dataurl <- paste0(loc_url, location.ids)
            results <- getGenomicsResponse(dataurl, F, logWarning = F, logError = T)
            hits <- data.frame()
            if(length(results) >= 1)
                hits <- rbind_pages(results)
            if(nrow(hits) == 0){
                next
            } else {
                df=(hits)
                df$admin_level[df$admin_level == "-1"] <- "World Bank Region"
                df$admin_level[df$admin_level == "0"] <- "country"
                df$admin_level[df$admin_level == "1"] <- "state/province"
                df$admin_level[df$admin_level == "1.5"] <- "metropolitan area"
                df$admin_level[df$admin_level == "2"] <- "county"
                df$full <- paste0(df$label, " (", df$admin_level, ")")
                for (i in df$full){
                    cat(paste0(i, "\n"))
                    loc_sel <- readline("Is this a location of interest? (Y/N): ")
                    if ((loc_sel == "Y")|(loc_sel == "y")){
                        locs_of_interest = c(locs_of_interest, df$id[df$full==i])
                        break
                    }
                    if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
                        cat("Expected input is Y or N\n\n")
                        cat(i)
                        loc_sel <- readline("Is this a location of interest? (Y/N): ")
                        if ((loc_sel == "Y")|(loc_sel == "y")){
                            locs_of_interest = c(locs_of_interest, df$id[df$full==i])
                            break
                        }
                    }
                }
            }
        }
    }
    return(locs_of_interest)
}
