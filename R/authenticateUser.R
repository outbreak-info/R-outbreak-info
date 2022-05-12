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
TERMS OF USE for R Package and

Reminder of GISAID's Database Access Agreement

Your ability to access and use Data in GISAID, including your access and
use of same via R Package, is subject to the terms and conditions of
GISAID's Database Access Agreement (“DAA”) (which you agreed to
when you requested access credentials to GISAID), as well as the
following terms:

1. You will treat all data contained in the R Package consistent with
other Data in GISAID and in accordance with GISAID's Database Access
Agreement;

2. You will not distribute, or re-distribute Data made available through
GISAID to any third party other than Authorized Users as contemplated by
the DAA;

3. USE OF R PACKAGE: Any visualizations, charts, graphs,
graphics, pictographs, plots, or other displays you create via the R
Package may be exclusively used for academic and research purposes.
No other types of uses are allowed;

4. Any use of visualizations, charts, graphs, graphics, pictographs,
plots, or other displays created via the R Package in an academic or
research publication, including in a paper, manuscript, preprint, website,
web service, or any other media material must be in conformity with the
GISAID Publishing Guidelines, available at https://www.gisaid.org/publish,
and the DAA, available at https://www.gisaid.org/daa; and

5. By using the R Package you reaffirm your understanding of these
terms and the DAA.

When using this data, please state, \"This data was obtained from GISAID via the outbreak.info API\". 
WE DO NOT SUPPORT THIRD PARTY APPLICATIONS. THIS PACKAGE IS MEANT FOR RESEARCH AND VISUALIZATION PURPOSES ONLY. 
If you want to build third party applications, please contact GISAID via https://www.gisaid.org/help/contact/.
"
    cat(red$bold(termsText))
}
