.BASE_URL <- "https://service.azul.data.humancellatlas.org"

.hca_path <- function(path) {
    stopifnot(
        is.character(path),
        length(path) == 1L,
        startsWith(path, "/")
    )
    message('request for:')
    message(paste0(.BASE_URL, path))
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

## pagination
.hca_next <-
    function(x)
{
    url <- x[["next"]]
    if (is.null(url))
        stop("already on the last page of results", call. = FALSE)
    .hca_GET_next_or_prev(url)
}

.hca_prev <-
    function(x)
{
    url <- x[["previous"]]
    if (is.null(url))
        stop("already on the first page of results", call. = FALSE)
    .hca_GET_next_or_prev(url)
}

.hca_GET_next_or_prev <-
    function (url)
{
    response <- GET(url)
    stop_for_status(response)
    list(
        content = content(response),
        status_code = response$status_code
    )
}

#' @rdname hca
#'
#' @title Page through HCA results
#'
#' @description `hca_next()` retrieves the next 'page' of results from
#'     a query of `projects()`, `samples()`, `files()`, or
#'     `bundles()`.
#'
#' @param x a 'tibble' or 'lol' object returned by `projects()`,
#'     `samples()`, `files()`, or `bundles()`.
#'
#' @return `hca_next()` returns the next page of results as a 'tibble'
#'     or 'lol'
#'
#' @examples
#' files <- files(size = 5)         # results 1-5, as a tibble
#'
#' next_files <- hca_next(files)    # results 6-10
#' next_files
#'
#' @export
hca_next <- function(x)
    UseMethod("hca_next")

#' @rdname hca
#'
#' @description `hca_prev()` returns the previous 'page' of results.
#'
#' @return `hcl_prev()` returns the previous page of results.
#'
#' @examples
#' hca_prev(next_files)             # previous results, i.e., files 1-5
#'
#' @export
hca_prev <- function(x)
    UseMethod("hca_prev")
