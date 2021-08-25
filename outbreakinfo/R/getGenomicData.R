#' @title Retrieve data from api.outbreak.info/genomics
#'
#' @description Retrieves up-to-date COVID-19 genomic data from outbreak.info according to user specifications.
#'
#'
#' @return dataframe
#'
#' @examples
#' getGenomicData(query_url="prevalence-by-location", location="United Kingdom", pangolin_lineage = "B.1.1.7")
#'
#' @export
#' @import jsonlite

getGenomicData <- function(query_url, location=NULL, cumulative=NULL, pangolin_lineage=NULL, mutations=NULL, ndays=NULL, frequency=NULL, subadmin=NULL, other_threshold=NULL, nday_threshold=NULL, other_exclude=NULL){
  genomic_url <- "http://localhost:8000/genomics/"

  q <- c()

  q <- c(q, paste0(query_url), "?")

  if(!is.null(location)){
    location <- getISO3_genomic(location)
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
    mutations <- paste(mutations, collapse=",")
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

  scroll.id <- NULL
  results <- list()
  success <- NULL
  while(is.null(success)){
      success <- FALSE
    cat("Retrieving data...", "\n")
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    tryCatch({
        resp <- GET(
            dataurl,
            add_headers(Authorization = paste("Bearer", Sys.getenv("OUTBREAK_INFO_TOKEN"), sep=" "))
        )
        auth_token = resp$headers$`x-auth-token`
        if(!is.null(auth_token))
            Sys.setenv(OUTBREAK_INFO_TOKEN = auth_token)
        if(resp$status_code == 401){
            warning("Please authenticate by calling authenticate() to access the API.")
        } else if (resp$status_code == 403) {
            warning("Invalid taken. Please reauthenticate by calling the authenticate() function.")
        } else if(resp$status_code == 500){
            warning("There was an internal server error. Please cross check your query or contact help@outbreak.info for further assistance.")
        } else if(resp$status_code == 429){
            warning("You have exceeded the API usage limit. Please limit the usage to 1 request/minute.")
        } else if (resp$status_code == 400){
            warning("Malformed token. Please reauthenticate by calling the authenticate() function.")
        } else if(resp$status_code == 200){
            resp <- fromJSON(content(resp, "text"), flatten=TRUE)
            results[[length(results) + 1]] <- resp$results
            scroll.id <- resp$'_scroll_id'
            success <- resp$success
        }
    }, error = function(cond){
        stop("Could not connect to API. Please check internet connection and try again.");
    }, warning = function(cond){
        message(cond)
    })
  }
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

df <- getGenomicData(query_url="sequence-count", location = "United States", cumulative = FALSE, subadmin = FALSE)
