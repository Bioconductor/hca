.PROJECTS_PATH <- "/index/projects"

#' @rdname projects
#'
#' @name projects
#'
#' @title HCA Project Querying
#'
#' @description `projects()` takes user input to be used to query the
#'     HCA API for information about available projects.
NULL # don't add next function to documentation

## helper functions
## internal only
.projects_parameters_path <- function(...) {
    params <- list(...)
    paste(names(params), unname(params), sep = "=", collapse = "&")
}

#' @importFrom jsonlite read_json
.projects_index_path <- function(parameters_path) {
    paste0(.PROJECTS_PATH, "?", parameters_path)
}

#' @param filters filter object as defined in this package
#'
#' @param size integer(1) maximum number of results to return;
#'     default: all projects matching `filter`. The default (10000) is
#'     meant to be large enough to return all results.
#'
#' @param sort character(1) project facet (see `facet_options()`) to
#'     sort result; default: `"projectTitle"`.
#'
#' @param order character(1) sort order. One of `"asc"` (ascending) or
#'     `"desc"` (descending).
#'
#' @param catalog character(1) source of data. Default: `"dcp2"`,
#'     version 2 of the HCA Data Coordinating Platform.
#'
#' @return `projects()` returns a tibble with each row representing an
#'     HCA project, and columns summarizing the project.
#'
#' @examples
#' projects()  # all projects
#'
#' @export
projects <-
    function(filters = filters(),
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"))
{
    ## validate
    size <- as.integer(size)
    sort <- match.arg(sort, facet_options())
    order <- match.arg(order) # defaults from argument
    catalog <- match.arg(catalog) # defults from argument
    stopifnot(
        `use 'filters()' to create 'filter=' argument` =
            inherits(filters, "filters"),
        length(size) == 1L
        ## sort, order, catalog already validated by match.arg
    )
    filters <- .filters_encoding(filters)

    ## parameters-as-list
    parameters_path <- .projects_parameters_path(
        filters = filters, size = size, sort = sort, order = order,
        catalog = catalog
    )

    projects_index_path <- .projects_index_path(parameters_path)

    ## FIXME: invoke content <- .hca_GET(path) from hca.R to do initial processing

    ## FIXME: invoke .projects_as_tibble(content)
}

.projects_as_tibble <- function(content)
{
    ## FIXME
}
