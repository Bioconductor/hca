.PROJECTS_PATH <- "/index/projects"
#' @rdname projects
#'
#' @title HCA Project Querying
#'
#' @description `projects()` takes user input to be used to query the HCA API.
#'     This function makes use of several other functions within this package.

## helper functions
## internal only
#' @importFrom magrittr %>%
.parameters_validate <- function(x) {
    ## validate parameters
    ## match.arg matches arg against a table of candidate values
    ## as specified by choices, where NULL means to take the first one.
    filters <- ifelse(!is.null(x$filters), x$filters, filters()$encoding)
    catalog <- match.arg(x$catalog, c("dcp2", "it2", "dcp1", "it1"))
    ## '100' is a numeric value, but we want integer
    size <- ifelse(!is.null(x$size), x$size, 10) %>%
        as.integer()
    sort <- ifelse(!is.null(x$sort), x$sort, "projectTitle")
    order <- match.arg(x$order, c("asc", "desc"))
    ## FIXME: better validation
    stopifnot(
        length(filters) == 1L, is.character(filters),
        length(catalog) == 1L, is.character(catalog),
        length(size) == 1L, is.integer(size),
        length(sort) == 1L, is.character(sort)
    )

    valid_params <- list(
        filters = filters,
        catalog = catalog,
        size = size,
        sort = sort,
        order  = order
    )
}
.parameters <- function(...) {

    params_list <- list(...)

    ## validate list of parameters
    valid_params <- .parameters_validate(params_list)

    paste(names(valid_params), unname(valid_params), sep = "=", collapse = "&")
}

#' @importFrom jsonlite read_json
.construct_url_path_validate <- function(x) {
    ## allowed paths
    api <- "https://service.azul.data.humancellatlas.org/openapi"
    json <- jsonlite::read_json(api)
    available_paths <- names(json$paths)

    ## validate arguments
    parameters <- ifelse(!is.null(x$parameters), x$parameters, .parameters())
    endpoint <- ifelse(!is.null(x$endpoint), x$endpoint, "/index/projects")

    stopifnot(
        ## FIXME: better validation
        length(parameters) == 1L, is.character(parameters),
        length(endpoint) == 1L, is.character(endpoint),
        ## allowed paths only
        `'construct_url_path()' paths must from the permissible list` =
            all(names(endpoint) %in% available_paths)
    )

    valid_url_path <- list(
        parameters = parameters,
        endpoint = endpoint
    )
}

.construct_url_path <- function(...) {
    url_path_list <- list(...)

    ## validate list of parameters
    valid_url_path <- .construct_url_path_validate(url_path_list)

    paste0(valid_url_path$endpoint, "?", valid_url_path$parameters)
}

#' @param filters filter object as defined in this package
#'
#' @param n integer(1) maximum number of results to return; default:
#'     all projects matching `filter`.
#'
#' @return `projects()` returns a tibble with each row representing an
#'     HCA project, and columns summarizing the project.
#'
#' @examples
#' projects(filters(), n=20)
#'
#' @export
projects <- function(filters = filters(),
                     n = Inf) {
    ## sort, order, etc need to be added as argument parameters
    ## FIXME: validate inputs

    ## FIXME: construct params <- .projects_params()

    ## FIXME: construct '/index/projects?<param>' using path <- .projects_path()

    ## FIXME: invoke content <- .hca_GET(path) from hca.R to do initial processing

    ## FIXME: invoke .projects_as_tibble(content)
}

.projects_param <- function() {
    ## FIXME
}

.projects_path <- function(param)
{
    paste0(.PROJECTS_PATH, "?", param)
}

.projects_as_tibble <- function(content)
{
    ## FIXME
}
