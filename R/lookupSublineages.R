#' Lookup sublineages for a given Pango lineage or WHO name
#'
#' @description Retrieve all sublineages of a given Pango lineage or WHO designation.
#' Based on a phylogeny maintained by the [Pango team](https://github.com/cov-lineages/lineages-website/blob/master/data/lineages.yml)
#' to grab all sublineages associated with a lineage, and a curated list of Variants of Concern, Variants of Interest,
#' Variants Under Monitoring, and de-escalated variants maintained by the [outbreak.info team](https://outbreak.info/situation-reports)
#'
#' @param lineage String with the name of a Pango lineage or WHO name
#' @param returnQueryString Boolean to return a query string to be piped into functions like \link[outbreakinfo]{getPrevalence} (collapses vector by `"OR"`)
#'
#' @import httr
#' @import yaml
#' @import dplyr
#' @return list containing the sublineages associated with that lineage or WHO designation
#' @export
#'
#' @examples
#' # WHO-designated lineages
#' lookupSublineages("epsilon")
#' lookupSublineages("epsilon", returnQueryString = TRUE)
#' # Pango lineage
#' lookupSublineages("B.1.1.7")
#' #' # Not a recognized lineage
#' lookupSublineages("VOC-21APR-02")

lookupSublineages = function (lineage, returnQueryString = FALSE) {
  curated = getCuratedLineages()
  who_lineage = curated %>% filter(tolower(who_name) == tolower(lineage)) %>% pull(pangolin_lineage)

  if(length(who_lineage) == 1){
    # WHO lineage; convert the lineage to a list
    children = lapply(who_lineage[[1]], getSublineages) %>% unlist()
  } else {
    children = getSublineages(lineage)
  }
  if(returnQueryString) {
    return(paste(children, collapse = " OR "))
  } else {
    return(children)
    }
}

getSublineages = function(lineage) {
  SUBLINEAGE_URL = "https://raw.githubusercontent.com/cov-lineages/lineages-website/master/data/lineages.yml"

  tryCatch({
    sublineages_resp = GET(SUBLINEAGE_URL)

    if(sublineages_resp$status_code != 200) {
      stop("Cannot access the Pango sublineages dictionary. Please contact the outbreak.info team at help@outbreak.info.")
    }

    sublineages = yaml.load(content(sublineages_resp, as="text"))

    filtered = sublineages[sapply(sublineages, function(x) x$name == toupper(lineage))]
    if(length(filtered) == 1) {
      children = filtered[[1]]$children
      return(children)
    } else {
      warning(paste("Lineage", lineage, "not recognized as a Pango lineage"))
    }

  }, error = function(cond){
    message(cond)
    stop("Cannot access the Pango sublineages dictionary. Please contact the outbreak.info team at help@outbreak.info.");
  }, warning = function(cond){
    message(cond)
  })
}
