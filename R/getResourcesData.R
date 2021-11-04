#' Get COVID-19 resource metadata
#'
#' @param query (optional) constructs a query over ALL fields, or a fielded query searching within specific fields. Fielded query terms should be separated by ` AND ` or ` OR `. See \href{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}{Elasticserach query strings} for more info.
#' @param types (optional) vector of resource type to return. The most frequent types include: "Publication", "ClinicalTrial", "Dataset", "Protocol", "SoftwareSourceCode", "Analysis".
#' @param size (optional) number of records to return (default = 10)
#' @param fetchAll (optional) Boolean whether to return all results for the query
#' @param fields (optional) vector specifying which fields to return. Returns all by default. See the \href{https://discovery.biothings.io/view/outbreak}{outbreak schema} for possible fields.
#' @param sort (optional) field to sort by.  Add `-` to sort in descending order
#' @param facets (optional) field by which to aggregate (count) their frequency
#' @param facet_size (optional) how many facet groups to include in the facet total (default = 10, max = 1000)
#' 
#' @import stringr
#' 
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' # Get the date latest date for every resource (newest of dateModified, dateCreated, datePublished)
#' resources_by_date = getResourcesData(query = "date:[2020-01-01 TO *]", fields = "date")
#' 
#' # Get all metadata on remdesivir
#' remdesivir = getResourcesData(query = "remdesivir", fetchAll = TRUE, fields = c("@type", "name", "curatedBy"))
#' remdesivir %>% count(`@type`) %>% arrange(desc(n))
#' 
#' # Get all metadata for remdesivir Clinical Trials or Datasets
#' remdesivir_trials_data = getResourcesData(query = "remdesivir", types = c("ClinicalTrial", "Dataset"), fetchAll = TRUE, fields = c("@type", "name", "curatedBy"))
#' remdesivir_trials_data %>% count(`@type`) %>% arrange(desc(n))

getResourcesData = function(query = NULL, types = NULL, size = 10, fetchAll = FALSE, fields = NULL, sort = NULL, facets = NULL, facet_size = 10) {
  
  # Append the `@type: filter` if selected
  if(!is.null(types)) {
    if(!is.null(query)) {
      query = str_c(query, " AND @type:(", paste(types, collapse = " OR "), ")")
    } else {
      query = str_c("@type:(", paste(types, collapse = " OR "), ")")
    }
  }
  
  return(getResourcesResponse(query, size, fetchAll, fields, sort, facets, facet_size))
}