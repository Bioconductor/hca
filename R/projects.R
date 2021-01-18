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

## extract a single element from a hit; returns a vector
.projects_elt <- function(hit, element) {
    ## different elements are found at different levels of the nested JSON
    ## using a switch statement
    switch (element,
        "projectTitle" = sapply(hit$projects,`[[`, "projectTitle"),
        "genusSpecies" = lapply(hit$donorOrganisms, `[[`, "genusSpecies"),
        "samplesOrgan" = lapply(hit$samples, `[[`, "organ"),
        "specimenOrgan" = lapply(hit$specimen, `[[`, "organ")
    )
}

## extract a single element from all hits; returns a list-of-vectors
.content_elt <- function(content, element) {
    lapply(content$hits, .projects_elt, element)
}

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate
#'
#' @importFrom tibble tibble
.projects_as_tibble <- function(content)
{
    tbl <-
        ## create tibble
        tibble(
            projectTitle = .content_elt(content, "projectTitle"),
            genusSpecies = .content_elt(content, "genusSpecies"),
            ## will need to investigate when, if ever, these differ
            samplesOrgan = .content_elt(content, "samplesOrgan"),
            specimenOrgan = .content_elt(content, "specimenOrgan")
        ) %>%
        ## add hit and project index
        mutate(
            .hit = seq_along(projectTitle),
            .project = lapply(lengths(projectTitle), seq_len)
        ) %>%
        ## unnest list columns
        unnest(c(
            "projectTitle", "genusSpecies", "samplesOrgan", "specimenOrgan",
            ".project"
        ))

    tbl
}

#' @param filters filter object created by `filters()`, or `NULL`
#'     (default; all projects).
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
#'     HCA project, and columns summarizing the project. Each `.hit`
#'     is a single result; a result may contain several projects, as
#'     indexed by `.project`.
#'
#' @examples
#' projects(filters())
#'
#' @export
projects <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"))
{
    if (is.null(filters))
        filters <- filters()
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

    response <- .hca_GET(projects_index_path)

    .projects_as_tibble(response$content)
}

