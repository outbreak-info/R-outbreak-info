#' Get response from genomics endpoint
#' @NoRd

#' A function rather aimed at developers
#' @description A function that does blabla, blabla.
#' @keywords internal
#' @export

getGenomicsResponse <- function(dataurl, logInfo = T, logWarning = T, logError = T){
    scroll.id <- NULL
    results <- list()
    success <- NULL
    while(is.null(success)){
        success <- FALSE
        if(logInfo){
            cat("Retrieving data...", "\n")
        }
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        dataurl <- URLencode(dataurl)
        resp <- NULL
        tryCatch({
            authToken <- getAuthToken()
            if(!is.null(authToken)){
                resp <- GET(
                    dataurl,
                    add_headers(Authorization = paste("Bearer", authToken, sep=" "))
                )
            } else {
                resp <- GET(
                    dataurl
                )
            }
            authToken = resp$headers$`x-auth-token`
            if(!is.null(authToken))
                setAuthToken(authToken)
            if(resp$status_code == 401){
                warning("Please authenticate by calling authenticateUser() to access the API.\n")
            } else if (resp$status_code == 403) {
                warning("Invalid token. Please reauthenticate by calling the authenticateUser() function.\n")
            } else if(resp$status_code == 500){
                warning("There was an internal server error. Please cross check your query or contact help@outbreak.info for further assistance.\n")
            } else if(resp$status_code == 429){
                warning("You have exceeded the API usage limit. Please limit the usage to 1 request/minute.\n")
            } else if (resp$status_code == 400){
                warning("Malformed token. Please reauthenticate by calling the authenticateUser() function.\n")
            } else if(resp$status_code == 200){
                resp <- fromJSON(content(resp, "text"), flatten=TRUE)
                if(length(resp$results) > 0) resp_df <- convert_list_to_dataframe(resp$results) else resp_df <- data.frame()
                results[[length(results) + 1]] <- resp_df
                scroll.id <- resp$'_scroll_id'
                success <- resp$success
                return(results)
            }
        }, error = function(cond){
            if(logError){
                message(cond)
            }
            stop("Could not connect to API. Please check internet connection and try again.")
        }, warning = function(cond){
            if(logWarning){
                message(cond)
            }
            return(NULL)
        })
    }
}

## If object is a list, return a long dataframe with query_key as new column.
## If object is a dataframe, return the object directly
## If object is neither, throw warning and return NULL
convert_list_to_dataframe <- function(list_obj){
    if(!(class(list_obj) %in% c("list", "data.frame"))){
        warning("Supplied object is not a list or dataframe")
        return(NULL)
    }
    if(class(list_obj) == "data.frame"){
        return(list_obj)
    }
    ## Exclude items in list that have 0 columns
    list_obj <- list_obj[sapply(list_obj, function(x){length(x) > 0})]
    ## If list add a "query_key" column
    query_keys <- names(list_obj)
    res <- lapply(query_keys,
                  function(query_key) {
                      d <- list_obj[[query_key]]
                      d <- d[!sapply(d, is.null)]
                      if(class(d) == "data.frame"){
                          d$query_key <- query_key
                      } else {
                          d <- data.frame(key = query_key, value = d)
                      }
                      return(d)
                  })
    res_df <- do.call(rbind, res)
    return(res_df);
}
