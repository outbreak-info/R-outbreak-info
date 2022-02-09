#' Set authentication token
#'
#' Writes authentication token to file
#'
#' @param token A String representing the token to be stored
#'
#' @noRd

setAuthToken <- function(token){
    wd <- getwd()
    tryCatch({
        fileConn<-file(paste0(wd, "/.R_outbreak-info-token"))
        writeLines(c(token), fileConn)
        close(fileConn)
    }, error = function(e){
        stop("Unable to write authentication token to disk. Please make sure your disk has space and the permissions are set correctly.")
    })
}