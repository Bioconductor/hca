#' @title Query filter construction function
#'
#' @description function for constructing the query url; defaults to querying
#'   Human Cell Atlas API
#'
#' @param r_filter list() query filter formatted as a nested list
#' @param base_url character(1)
#' @param endpoint character(1)
#' @param params list() additional query parameters formatted as a nested list
#'
#' @return character(1) url for API call
#' @export
#'
#' @importFrom dplyr %>%
construct_query_filter <- function(
                        r_filter = list(),
                        base_url = "https://service.dev.singlecell.gi.ucsc.edu",
                        endpoint = "/index/projects", params = list()){
                        ## takes list and makes it json
                        json_filter <- jsonlite::toJSON(r_filter)

    ## need to encode url so that curly braces are not escaped,
    ## but treated as legal characters
    encoded_filter <- utils::URLencode(json_filter, reserved = TRUE)

    ## add encoded filter to params
    ## help from: https://www.datamentor.io/r-programming/list/
    params[["filters"]] <- encoded_filter


    ## further parameter processing; do names(params) to get parameter key names
    param_key_value <- paste(names(params), params, sep="=")

    ## format the query by collapsing into single string separated by `&`
    query <- paste(param_key_value, collapse = "&")

    ## form the URL for the GET query
    ## paste with no sep
    ## url automatically returned as it is the last statement in the function
    url <- paste0(
        base_url,
        endpoint,
        "?",
        query
    )
    ## return(url)
}