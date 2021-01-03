#' @rdname filters
#'
#' @title FIXME
#'
#' @description FIXME
#'
#' @param ... named arguments, each of which is a `list()` specifying
#'     ... FIXME
#'
#' @return `filters()` returns a `filters` object representing
#'     validated filters in a format suitable for use in `projects()`
#'     and related functions.
#'
#' @examples
#' filters()
#'
#' filters(
#'     organ = list(is = "pancreas")
#' )
#'
#' filters(
#'     organ = list(is = "pancreas"),
#'     genusSpecies = list(is = "Homo sapiens")
#' )
#'
#' filters(
#'     fileFormat = list(is = c("fastq", "fastq.gz"))
#' )
#'
#' @importFrom jsonlite toJSON
#'
#' @export
filters <-
    function(...)
{
    filters <- list(...)

    ## FIXME: validate list of filters

    ## FIXME: transform to json, via toJSON()
    ## do elements of `filters` need to be wrapped in `list()`?
    ## ??? filters <- lapply(filters list) ???
    json <- character()

    ## create a 'filters' object (S3 for now)
    result <- list(json = json, filters = filters)
    class(result) <- c("filters", "HCAccess")

    ## return result
    result
}

## accessors, not exported
.filters_json <- function(x) x$json

.filters_filters <- function(x) x$filters

#' @rdname filters
#'
#' @param x for `length()` and `print()`, an object of class `filters`.
#'
#' @export
length.filters <-
    function(x)
{
    length(.filters_filters(x))
}

#' @rdname filters
#'
#' @export
print.filters <-
    function(x, ...)
{
    cat(
        "class: ", paste(class(x), collapse = ", "), "\n",
        "length: ", length(x), "\n",
        "json: ", .filters_json(x), "\n",
        sep = ""
    )
}
