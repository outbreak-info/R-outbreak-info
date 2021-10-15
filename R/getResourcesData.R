#' Get COVID-19 resource metadata
#'
#' @param query (optional): constructs a query over ALL fields, or a fielded query searching within specific fields. Fielded query terms should be separated by ` AND ` or ` OR `. See Elasticserach query strings for more info.
#' @param types (optional): vector of resource type 
#' @param size (optional): number of records to return (default = 10)
#' @param fetchAll (optional): Boolean whether to return all results for the query
#' @param fields (optional): vector specifying which fields to return. Returns all by default
#' @param sort (optional): field to sort by.  Add `-` to sort in descending order
#' @param facets (optional): field by which to aggregate (count) their frequency
#' @param facet_size (optional): how many facet groups to include in the facet total (default = 10)
#' 
#' @import stringr
#' 
#' @return
#' @export
#'
#' @examples
#' # Get the date latest date for every resource (newest of dateModified, dateCreated, datePublished)
#' resources_by_date = getResourcesData(query = "date:[2020-01-01 TO *]", fields = "date")

getResourcesData = function(query = NULL, types = NULL, size = 10, fetchAll = FALSE, fields = null, sort = NULL, facets = NULL, facet_size = 10) {
  
  # Append the `@type: filter` if selected
  if(!is.null(types)) {
    if(!is.null(query)) {
      query = str_c(" AND @type=", paste(types, sep = " OR "))
    } else {
      query = str_c("@type=", paste(types, sep = " OR "))
    }
  }
  
  return(getResourcesResponse(query, size, fetchAll, fields, sort, facets, facet_size))
}