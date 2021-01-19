.FILES_PATH <- "/index/files"

#' @rdname files
#'
#' @name files
#'
#' @title HCA File Querying
#'
#' @description `files()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available files.

NULL # don't add next function to documentation

## helper functions
## internal only

## extract a single element from a hit; returns a vector
.files_elt <- function(hit, element) {
    ## different elements are found at different levels of the nested JSON
    ## using a switch statement
    switch (element,
            "projectTitle" = sapply(hit$projects,`[[`, "projectTitle"),
            "genusSpecies" = sapply(hit$donorOrganisms, `[[`, "genusSpecies"),
            "samplesOrgan" = sapply(hit$samples, `[[`, "organ"),
            "specimenOrgan" = sapply(hit$specimen, `[[`, "organ"),
            "fileName" = lapply(hit$files,`[[`, "name"),
            "libraryConstructionApproach" =
                hit$protocols[[1]][["libraryConstructionApproach"]],
            "uuid" = lapply(hit$files,`[[`, "uuid")
    )
}

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate
#'
#' @importFrom tibble tibble
.files_as_tibble <- function(content) {
    tbl <-
        ## create tibble
        tibble(
            projectTitle = .content_elt(content, .files_elt, "projectTitle"),
            genusSpecies = .content_elt(content, .files_elt, "genusSpecies"),
            ## will need to investigate when, if ever, these differ
            samplesOrgan = .content_elt(content, .files_elt, "samplesOrgan"),
            specimenOrgan = .content_elt(content, .files_elt, "specimenOrgan"),
            fileName = .content_elt(content, .files_elt, "fileName"),
            libraryConstructionApproach =
                .content_elt(content,.files_elt, "libraryConstructionApproach"),
            uuid = .content_elt(content, .files_elt, "uuid")

        ) %>%
        ## add hit and project index
        mutate(
            ## .data is the data frame being passed
            .hit = seq_along(.data$projectTitle),
            .project = lapply(lengths(.data$projectTitle), seq_len)
        ) %>%
        ## unnest list columns
        unnest(c(
            "projectTitle", "fileName", "genusSpecies", "samplesOrgan",
            "specimenOrgan", "libraryConstructionApproach", "uuid", ".project"
        ))

    tbl
}

#' @param projectTitles a vector of one or more strings corresponding to project
#'     titles for which you want to review files
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
#' @return `files()` returns a tibble with each row representing a file for one
#'     of the specified HCA project, and columns summarizing the file.
#'     Each `.hit` is a single result; ....
#' @export
#'
#' @examples
#' files(c("Tabula Muris: Transcriptomic characterization of 20 organs and
#' tissues from Mus musculus at single cell resolution", "A Single-Cell
#' Transcriptomic Map of the Human and Mouse Pancreas Reveals Inter- and
#' Intra-cell Population Structure"))
files <- function(projectTitles = NULL,
                  size = 1000L,
                  sort = "projectTitle",
                  order = c("asc", "desc"),
                  catalog = c("dcp2", "it2", "dcp1", "it1")) {
    ## validate
    size <- as.integer(size)
    sort <- match.arg(sort, facet_options())
    order <- match.arg(order) # defaults from argument
    catalog <- match.arg(catalog) # defaults from argument

    stopifnot(
        ## 'projectTitles' must be a vector
        is.vector(projectTitles),
        ## 'projectTitles' must have at least one entry
        length(projectTitles) > 0L
    )

    file_filters <- filters(projectTitle = list(is = projectTitles))

    file_filters <- .filters_encoding(file_filters)

    ## parameters-as-list
    files_parameters_path <- .parameters_path(
        filters = file_filters, size = size, sort = sort, order = order,
        catalog = catalog
    )

    files_index_path <- .index_path("/index/files", files_parameters_path)

    response <- .hca_GET(files_index_path)

    .files_as_tibble(response$content)

}