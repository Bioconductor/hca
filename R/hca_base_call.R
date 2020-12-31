#' @title Full Human Cell Atlas API call function
#' 
#' @description function is composed of the construct_query_filter() and
#' hca_api_call() functions, taking in API call parameters and returning a response
#'
#' @param r_filter list() query filter formatted as a nested list
#' @param base_url character(1)
#' @param endpoint character(1)
#' @param params list() additional query parameters formatted as a nested list 
#'
#' @return list() response headers and content from call, error message outpt if needed
#' @export
hca_base_call <- function(r_filter = list(), base_url = "https://service.dev.singlecell.gi.ucsc.edu", endpoint = "/index/projects", params = list()){
    url <- construct_query_filter(r_filter, base_url, endpoint, params)
    resp <- hca_api_call(url)
    #return(resp)
}