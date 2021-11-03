#' Lookup curated lineages from outbreak.info
#'
#' @description Retrieve a curated list of Variants of Concern, Variants of Interest,
#' Variants Under Monitoring, and de-escalated variants maintained by the [outbreak.info team](https://outbreak.info/situation-reports)
#'
#'
#' @import httr
#' @import jsonlite
#' @return dataframe containing VOCs, VOIs, VUMs, and associated metadata.
#' @export
#'
#' @examples
#' curated = getCuratedLineages()
#' # Pull out the curated lineages which are WHO-desginated
#' curated[!is.na(curated$who_name),]
#' # Pull out the Variants of Concern
#' curated[curated$variantType == "Variant of Concern",]


getCuratedLineages = function() {
  ALIAS_URL = "https://raw.githubusercontent.com/outbreak-info/outbreak.info/master/web/src/assets/genomics/curated_lineages.json"

  tryCatch({
    curated_resp = GET(ALIAS_URL)


    if(curated_resp$status_code != 200) {
      stop("Cannot access the Pango sublineages dictionary. Please contact the outbreak.info team at help@outbreak.info.")
    }

    curated = fromJSON(content(curated_resp, as="text"))
    return(curated)
  }, error = function(cond){
    message(cond)
    stop("Cannot access the curated lineages dataframe. Please contact the outbreak.info team at help@outbreak.info.");
  }, warning = function(cond){
    message(cond)
  })
}
