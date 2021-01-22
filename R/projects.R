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
.content_elt <- function(content, func, element) {
    lapply(content$hits, func, element)
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
            projectTitle = .content_elt(content, .projects_elt, "projectTitle"),
            genusSpecies = .content_elt(content, .projects_elt, "genusSpecies"),
            ## will need to investigate when, if ever, these differ
            samplesOrgan = .content_elt(content, .projects_elt, "samplesOrgan"),
            specimenOrgan = .content_elt(content, .projects_elt,
                                         "specimenOrgan")
        ) %>%
        ## add hit and project index
        mutate(
            ## .data is the data frame being passed
            .hit = seq_along(.data$projectTitle),
            .project = lapply(lengths(.data$projectTitle), seq_len)
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
    function(filters,
             size = 1000L,
             sort = "projectTitle",
             order = NULL,
             catalog = NULL) {

    if (is.null(filters))
        filters <- filters()

    response <- .index_GET(filters = filters,
                           size = size,
                           sort = sort,
                           order = order,
                           catalog = catalog,
                           base_path = .PROJECTS_PATH)

    .projects_as_tibble(response$content)
}

