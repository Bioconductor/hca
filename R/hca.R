.BASE_URL <- "https://service.azul.data.humancellatlas.org"
## internal only
.hca_GET_validate <- function(x) {
    stopifnot(
        length(x) == 1L,
        is.character(x),
        startsWith(x, "https://"),
        httr::http_error(x) == FALSE
    )
    x
}

## internal only
#' @importFrom httr GET stop_for_status headers content
.hca_GET <- function(path) {
    uri <- paste0(.BASE_URL, path)

    valid_uri <- .hca_GET_validate(uri)
    response <- GET(valid_uri)
    stop_for_status(response)

    resp <- list(
        content = content(response),
        status_code = status_code(response)
    )
}
