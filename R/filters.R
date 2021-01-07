#' @rdname filters
#'
#' @title HCA Filter Construction
#'
#' @description This set of functions takes user input to be used as query
#' filters, ideally in the form of nested lists. This input is then validated,
#' reformatted to JSON, and encoded into a properly formatted URL.

## other helper functions
.filters_validate <- function(x) {
    ## print(names(x))
    stopifnot(
        ## 'filter' must be a list
        is.list(x),
        ## length 0 or elements must have names
        length(x) == 0L || !is.null(names(x))
        ## each element in the list must also be a list
        ## appropriate verbs only
        ## i.e must be one of "is", "within", "intersects", or "contains"
    )

    x
}

#' @param ... named arguments, each of which is a `list()` specifying a query
#' facet and its corresponding value to be used in the query
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
#' @importFrom utils URLencode
#'
#' @export
filters <- function(...) {
    filters_list <- list(...)

    ## make sure zero-length filters have names
    if (length(filters_list) == 0L) {
        names(filters_list) <- character()
    }

    ## validate list of filters
    valid_filters <- .filters_validate(filters_list)

    ## transform to json, via toJSON()
    ## do elements of `filters` need to be wrapped in `list()`?
    ## ??? filters <- lapply(filters list) ???
    json <- toJSON(valid_filters)
    encoded <- URLencode(json, reserved = TRUE)

    result <- list(json = encoded, filters = valid_filters)

    ## create a 'filters' object (S3 for now)
    ## S3 classes have simple linear inheritance, and `c("filters","HCAccess")`
    ## says that the `filters` class is a subclass of `HCAccess`.
    ## It doesn't matter that `HCAcces` is not defined anywhere.
    class(result) <- c("filters", "HCAccess")

    ## return result
    result
}

## class accessors, not exported
.filters_json <- function(x) x$json

.filters_filters <- function(x) x$filters

## @rdname is used to document more than one function in the same file
## the syntax convention for naming class methods is <generic>.<class>
## where a generic function is just an intermediary which determines which
## implementation of a function to call based on the class of its input

#' @rdname filters
#'
#' @param x for `length()` and `print()`, an object of class `filters`.
#'
#' @export
length.filters <- function(x) {
    ## length() is a generic which chooses its implementation based on the class
    ## of its input
    length(.filters_filters(x))
}

#' @rdname filters
#'
#' @export
print.filters <- function(x, ...) {
    ## cat() is an internal function which concatenates inputs and prints
    ## print() is also a generic which chooses its implementation based
    ## on the class of its input

    cat(
        "class: ", paste(class(x), collapse = ", "), "\n",
        "length: ", length(x), "\n",
        "json: ", .filters_json(x), "\n",
        sep = ""
    )
}
