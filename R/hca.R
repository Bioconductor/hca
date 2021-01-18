.BASE_URL <- "https://service.azul.data.humancellatlas.org"

.hca_path <- function(path) {
    stopifnot(
        is.character(path),
        length(path) == 1L,
        startsWith(path, "/")
    )
    paste0(.BASE_URL, path)
}

## local cache of API specification, updated once / hour
.hca_openapi <- local({
    json <- NULL
    timestamp <- NULL
    function() {
        if (is.null(timestamp) || Sys.time() > timestamp + 3600L) {
            api <- .hca_path("/openapi")
            json <<- jsonlite::read_json(api)
            timestamp <<- Sys.time()
        }
        json
    }
})

## internal only
#' @importFrom httr GET stop_for_status headers content
.hca_GET <- function(path) {
    uri <- .hca_path(path)

    response <- GET(uri)
    stop_for_status(response)
    list(
        content = content(response),
        status_code = response$status_code
    )
}
