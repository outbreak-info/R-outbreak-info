OUTBREAK_INFO_AUTH = "https://api.outbreak.info/genomics/get-auth-token"

#' @title Authenticate API
#'
#' @description Authenticate API to get access to genomics data
#'
#' @import httr
#'
#' @return url
#'
#' @examples
#'# Authenticate with GISAID credentials
#'# authenticateUser()
#' @export
authenticateUser <- function(){
    response <- POST(OUTBREAK_INFO_AUTH, body = '{}', encode = "raw")
    if (status_code(response) == 200) {
        response_content <- content(response)
        authToken <- response_content$authn_token
        setAuthToken(authToken)
        cat(paste("Please open this url in a browser and authenticate with your GISAID credentials.", response_content$authn_url, sep="\n\n"))
        browseURL(response_content$authn_url)
    } else {
        print("Could not get authentication-token")
    }
    start_time <- Sys.time()
    Sys.sleep(5)
    while (TRUE) {
        cat("\n\nWaiting for authentication... [press CTRL-C to abort]")
        response <- GET(
            OUTBREAK_INFO_AUTH,
            add_headers(Authorization = paste("Bearer", getAuthToken(), sep=" "))
        )
        if (response$status_code == 200){
            cat("\nAuthenticated successfully!\n")
            authToken = response$headers$`x-auth-token`
            if(!is.null(authToken))
                setAuthToken(authToken)
            break
        } else if (response$status_code == 403){
            cat("\nAuthentication failed!\nTrying again in 5 seconds ... \n")
        } else {
            cat("\nAuthentication error!\nTrying again in 5 seconds ... \n")
        }
        Sys.sleep(5)
        if(as.numeric(Sys.time() - start_time) > 60){
            cat("\nAborting. Please try again.\n")
            break;
        }
    }
}
