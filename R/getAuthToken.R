#' Get authentication token
#'
#' Get authentication token from file
#'
#' @noRd

getAuthToken <- function(token){
    wd <- getwd()
    token_file <- paste0(wd, "/.R_outbreak-info-token")
    if(file.exists(token_file)){
        con <- file(token_file, "r")
        token <- readLines(token_file)
        close(con)
        if(length(token) != 1){
            return(NULL);
        }
        return(token)
    } else {
        if(Sys.getenv("OUTBREAK_INFO_TOKEN") != ""){
            return(Sys.getenv("OUTBREAK_INFO_TOKEN"))
        }
    }
    return(NULL)
}