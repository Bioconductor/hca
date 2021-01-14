.BASE_URL <- "https://service.azul.data.humancellatlas.org"

.hca_path <- function(path) {
    stopifnot(
        is.character(path),
        length(path) == 1L,
        startsWith(path, "/")
    )
    paste0(.BASE_URL, path)
}

## internal only
#' @importFrom httr GET stop_for_status headers content
.hca_GET <- function(path) {
    uri <- .hca_path(path)

    response <- GET(uri)
    stop_for_status(response)

    resp <- list(
        content = content(response),
        status_code = status_code(response)
    )
}
