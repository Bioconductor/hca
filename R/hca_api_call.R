#' @title Human Cell Atlas API call
#' 
#' @description function makes a GET request to the Human Cell Atlas API using the provided url string
#'
#' @param url character(1) url string for API call
#'
#' @return list() response headers and content from call, error message outpt if needed
#' @export
hca_api_call <- function(url = ''){
    response <- httr::GET(url)
    tryCatch({
        httr::stop_for_status(response) # make sure response code isn't 400 or higher
    }, error = function(e) {
        ## convert error into a simple 'message' so execution continues
        message(e)
    })
    
    # help returning multiple values: https://stackoverflow.com/questions/8936099/returning-multiple-objects-in-an-r-function
    
    headers <- httr::headers(response) %>%
                 utils::str()
    content <- httr::content(response)
    
    resp <- list("headers" = headers, "content" = content)
    #return(resp)
}