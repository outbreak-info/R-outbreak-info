#' @title Retrieve data from api.outbreak.info/genomics
#'
#' @description Retrieves up-to-date COVID-19 genomic data from outbreak.info according to user specifications.
#'
#'
#' @return dataframe
#' @export
#'
#' @examples
#' uk_b117 = getGenomicData(query_url="prevalence-by-location", location="United Kingdom", pangolin_lineage = "B.1.1.7")
#' head(uk_b117)

getGenomicData <- function(query_url, location=NULL, cumulative=NULL, pangolin_lineage=NULL, mutations=NULL, ndays=NULL, frequency=NULL, subadmin=NULL, other_threshold=NULL, nday_threshold=NULL, other_exclude=NULL, logInfo=TRUE){

    genomic_url <- "https://api.outbreak.info/genomics/"

    q <- c()

    q <- c(q, paste0(query_url), "?")

    if(!is.null(location)){
        location <- getLocationIdGenomic(location)
        if(length(location) == 0){
            cat(paste0("Could not find location ", location, "\n"))
            return(NULL)
        }
        q <- c(q, paste0("location_id=", location, "&"))
    }
    if(!is.null(cumulative)){
        if (!is.logical(cumulative)){
            stop("cumulative must be in Boolean format")
        }else{
            q <- c(q, paste0("cumulative=", tolower(cumulative)), "&")
        }
    }
    if(!is.null(subadmin)){
        if (!is.logical(subadmin)){
            stop("subadmin must be in Boolean format")
        }else{
            q <- c(q, paste0("subadmin=", tolower(subadmin)), "&")
        }
    }
    if(!is.null(pangolin_lineage)){
        q <- c(q, paste0("pangolin_lineage=", pangolin_lineage, "&"))
    }
    if(!is.null(mutations)){
        check_cond <- grepl("[A-Za-z0-9]+:[A-Za-z][0-9]+[A-Za-z]", mutations)
        if(!all(check_cond))
            warning(paste0("Mutations should be specified in the format gene:mutation, like \"S:E484K\". The following mutations are not in the specified format: ",  paste(mutations[!check_cond], collapse=", ")))
        mutations <- paste(mutations, collapse=" AND ")
        q <- c(q, paste0("mutations=", mutations, "&"))
    }
    if(!is.null(ndays)){
        q <- c(q, paste0("ndays=", ndays, "&"))
    }
    if(!is.null(frequency)){
        q <- c(q, paste0("frequency=", frequency, "&"))
    }
    if(!is.null(other_threshold)){
        q <- c(q, paste0("other_threshold=", other_threshold, "&"))
    }
    if(!is.null(nday_threshold)){
        q <- c(q, paste0("nday_threshold=", nday_threshold, "&"))
    }
    if(!is.null(other_exclude)){
        other_exclude <- paste(other_exclude, collapse=",")
        q <- c(q, paste0("other_exclude=", other_exclude, "&"))
    }

    q <- paste(q, sep="", collapse = "")
    q <- sub("&$", "", q)

    dataurl <- paste0(genomic_url, q)
    results <- getGenomicsResponse(dataurl, logInfo);

    if (length(results) > 1){
        hits <- rbind_pages(results)
    }else{
        hits <- data.frame(results)
    }
    if ("date" %in% colnames(hits)){
        hits$date=as.Date(hits$date, "%Y-%m-%d")
        hits <- hits[order(as.Date(hits$date, format = "%Y-%m-%d")),]
    }
    return(hits)
}
