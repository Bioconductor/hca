.BASE_URL <- "https://service.azul.data.humancellatlas.org"

#' @importFrom httr GET stop_for_status headers content
.hca_GET <-
    function(path)
{
    uri <- paste0(.BASE_URL, path)

    response <- GET(uri)
    stop_for_status(response)

    content(response)
}        
