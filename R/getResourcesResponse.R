#' Access outbreak.info Research Library API
#'
#' @param query (optional) constructs a query over ALL fields, or a fielded query searching within specific fields. Fielded query terms should be separated by ` AND ` or ` OR `. See \href{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}{Elasticserach query strings} for more info.
#' @param size (optional) number of records to return (default = 10)
#' @param fetchAll (optional) Boolean whether to return all results for the query
#' @param fields (optional) vector specifying which fields to return. Returns all by default
#' @param sort (optional) field to sort by.  Add `-` to sort in descending order
#' @param facets (optional) field by which to aggregate (count) their frequency
#' @param facet_size (optional) how many facet groups to include in the facet total (default = 10, max = 1000)
#'
#' @import httr
#' @import RcppSimdJson
#' @import progress
#' @import stringr
#' @import dplyr
#'
#' @return dataframe or list (for facetted queries) containing the response of the API query
#' @export
#'
#' @examples
#' # Return only the first 10 hits for "ivermectin"
#' df = getResourcesResponse("ivermectin", size = 10)
#' 
#' # Find the first 10 results which contain the variable `topicCategory`  
#' df = getResourcesResponse("_exists_:topicCategory", size = 10)
#' 
#' # Get `date`/`name` from the first 10 hits for the query "ivermectin" plus a count of the `@type` field for ALL results
#' df = getResourcesResponse("ivermectin", size = 10, facets="@type", fields=c("date", "name"))

getResourcesResponse = function(queryString = NULL, size = 10, fetchAll = FALSE, fields = NULL, sort = NULL, facets = NULL, facet_size = 10, queryStub = "https://api.outbreak.info/resources/query?") {
  
  # Construct the query
  query = queryStub
  
  # add base query
  if(!is.null(queryString)){
    query = str_c(queryStub, "q=", queryString)
  }
  
  
  # add sorting
  if(!is.null(sort)){
    query = str_c(query, "&sort=", sort)
  }
  
  # add size param
  if(!is.null(size)){
    query = str_c(query, "&size=", size)
  }
  
  # add fields param
  if(!is.null(fields)){
    query = str_c(query, "&fields=", paste(fields, collapse=","))
  }
  
  # add facets param
  if(!is.null(facets)){
    query = str_c(query, "&facets=", facets, "&facet_size=", facet_size)
  }
  
  
  # --- RUN THE QUERY(-IES) --
  if(fetchAll) {
    # Fetch all queries -- keep going till you hit the end
    query = str_c(query, "&fetch_all=true")
    
    df = tibble()
    res = getResourcesQuery(query)
    
    # add progress bar
    pb <- progress_bar$new(total = res$total, clear = FALSE, show_after = 0)
    
    while(!is.null(res)){
      pb$update(nrow(df) / res$total)
      df = df %>% bind_rows(res$hits)
      res = getResourcesQuery(query, res$id)
    }
    
    pb$update(1)
    pb$terminate()
    
    # return statement
    if(!is.null(facets)) {
      results = list(hits = df, total = res[["total"]], facets = res[["facets"]])
    } else {
      results = list(hits = df)
    }
    
    
  } else {
    # Single query
    results = getResourcesQuery(query)
  }
  
  # Data cleanup!
  # remove `_score`
  if(!is.null(results$hits)){
    results$hits = results$hits %>%
      select(-`_score`)
  }
  
  if("date" %in% colnames(results$hits)) {
    results$hits = results$hits %>%
      mutate(date =  as.Date(date, "%Y-%m-%d"))
  }
  
  if("datePublished" %in% colnames(results$hits)) {
    results$hits = results$hits %>%
      mutate(datePublished =  as.Date(datePublished, "%Y-%m-%d"))
  }
  
  if("dateModified" %in% colnames(results$hits)) {
    results$hits = results$hits %>%
      mutate(dateModified =  as.Date(dateModified, "%Y-%m-%d"))
  }
  
  
  if("dateCreated" %in% colnames(results$hits)) {
    results$hits = results$hits %>%
      mutate(dateCreated =  as.Date(dateCreated, "%Y-%m-%d"))
  }
  
  
  if("dateCompleted" %in% colnames(results$hits)) {
    results$hits = results$hits %>%
      mutate(dateCompleted =  as.Date(dateCompleted, "%Y-%m-%d"))
  }
  
  # Flatten the results, if they don't include an aggregation
  if(!is.null(facets)) {
    return(results)
  } else {
    return(results$hits)
  }
  
}

# helper function to grab the results
getResourcesQuery = function(query, scroll_id = NA) {
  if(!is.na(scroll_id)){
    query = str_c(query, "&scroll_id=", scroll_id)
  }
  resp = GET(URLencode(query))
  
  if(resp$status_code == 200) {
    resp_content = content(resp, as="text")
    results = fparse(resp$content)
    if(length(results[["success"]] == 0)) {
      return(NULL)
    } else {
      return(list(hits = results[["hits"]], id = results[["_scroll_id"]], total = results[["total"]], facets = results[["facets"]]))
    }
  } else if(resp$status_code == 429) {
    warning("You have exceeded the API usage limit. Please limit the usage to 1 request/minute.\n")
    return(NULL)
  } else if(resp$status_code == 500){
    stop("There was an internal server error. Please check your query (", query, ") or contact help@outbreak.info for further assistance.\n")
  }
  else {
    stop("Hmm. Some unknown error happened. Please reach out to help@outbreak.info for help.")
    return(NULL)
  }
}
