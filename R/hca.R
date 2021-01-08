.BASE_URL <- "https://service.azul.data.humancellatlas.org"
## helper functions
.parameters_validate <- function(x) {
    ## validate parameters
    ## FIXME: better validation
    stopifnot(
        length(x$filter) == 1L, is.character(x$filter),
        length(x$catalog) == 1L, is.character(x$catalog),
        length(x$size) == 1L, is.integer(x$size),
        length(x$sort) == 1L, is.character(x$sort)
    )

    x
}
.parameters <- function(filter = filters(),
                        catalog = c("dcp2", "it2", "dcp1", "it1"),
                        size = 100,
                        sort = "projectTitle",
                        order = c("asc", "desc")) {

    ## match.arg() validates that 'order' provided by the user is one
    ## of the vector of default values in the function argument; if
    ## 'order' is not specified by the user, then the first ('asc') is
    ## used. 'order' can be considered to have been validated after this step.
    ## same goes for catalog
    order <- match.arg(order)
    catalog <- match.arg(catalog)

    ## '100' is a numeric value, but we want integer...
    size <- as.integer(size)

    params_list <- list(
        catalog = catalog,
        filter = filter,
        size = size,
        sort = sort,
        order = order
    )

    ## validate list of parameters
    valid_params <- .parameters_validate(params_list)

    paste(names(valid_params), unname(valid_params), sep = "=", collapse = "&")
}

.construct_url_path_validate <- function(x) {
    ## validate arguments
    stopifnot(
        ## FIXME: better validation
        length(x$parameters) == 1L, is.character(x$parameters),
        length(x$endpoint) == 1L, is.character(x$endpoint)
    )

    x
}

.construct_url <- function(parameters = .parameters(),
                             endpoint = "/index/projects") {
    url_path_list <- list(
        parameters = parameters,
        endpoint = endpoint
    )

    ## validate list of parameters
    valid_url_path <- .construct_url_path_validate(url_path_list)

    paste0(valid_url$endpoint, "?", valid_url$parameters)
}

.hca_GET_validate <- function(x) {
    stopifnot(
        length(x) == 1L, is.character(x), startsWith(x, "https://")
    )

    x
}

#' @importFrom httr GET stop_for_status headers content
.hca_GET <- function(path) {
    uri <- paste0(.BASE_URL, path)

    valid_uri <- .hca_GET_validate(uri)
    response <- GET(valid_uri)
    stop_for_status(response)

    content(response)
}
