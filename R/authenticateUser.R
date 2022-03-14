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
            printTerms()
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

printTerms <- function(){
    termsText <- "
NOTICE AND REMINDER of TERMS OF USE:

The Terms of Use you agreed to when requesting access credentials to GISAID include the following terms:
1.	You will not distribute data made available through GISAID (“Data in GISAID”) to any third party other than Authorized Users as contemplated by GISAID's Database Access Agreement;
2.	You will not display the data in any form in a publication, preprint, manuscript, or any other media material including on a website, without additional express written permission from GISAID;
3.	You will treat all data contained in these files consistent with other Data in GISAID and in accordance with GISAID's Database Access Agreement; and
4.	You will provide proper attributions, acknowledgements, and efforts to collaborate with data submitters consistent with the GISAID's and Database Access Agreement when using Data in GISAID in publications, preprints, manuscripts, or any other analyses or media material.

You can see the full terms of use at https://www.gisaid.org/registration/terms-of-use/. By using the outbreak.info R package you reaffirm your understanding of these terms and the DAA.

When using this data, please state, \"This data was obtained from GISAID via the outbreak.info API\". WE DO NOT SUPPORT THIRD PARTY APPLICATIONS. THIS PACKAGE IS MEANT FOR RESEARCH AND VISUALIZATION PURPOSES ONLY. If you want to build third party applications, please contact GISAID via https://www.gisaid.org/help/contact/.
"
    cat(termsText)
}
