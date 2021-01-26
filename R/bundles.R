.BUNDLES_PATH <- "/index/bundles"

#' @rdname bundles
#'
#' @name bundles
#'
#' @title HCA File Querying
#'
#' @description `bundles()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available bundles.

NULL # don't add next function to documentation

## helper functions
## internal only
.bundle_samples <- function(samples) {
    samp_out <- c()

}

.bundle_files <- function(files) {

}

## extract a single element from a hit; returns a vector
.bundles_elt <- function(hit, element) {
    ## different elements are found at different levels of the nested JSON
    ## using a switch statement
    switch (element,
            "projectTitle" = sapply(hit$projects,`[[`, "projectTitle"),
            "genusSpecies" = sapply(hit$donorOrganisms, `[[`, "genusSpecies"),
            "samples" = sapply(hit$samples, `[[`, "id"),
            "files" = sapply(hit$files, `[[`, "name"),
            "uuid" = sapply(hit$bundles,`[[`, "bundleUuid"),
            "version" = sapply(hit$bundles,`[[`, "bundleVersion")
    )
}

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate
#'
#' @importFrom tibble tibble
.bundles_as_tibble <- function(content) {
    tbl <-
        ## create tibble
        tibble(
            projectTitle = .content_elt(content, .bundles_elt, "projectTitle"),
            genusSpecies = .content_elt(content, .bundles_elt, "genusSpecies"),
            samples = .content_elt(content, .bundles_elt, "samples"),
            files = .content_elt(content, .bundles_elt, "files"),
            bundleUuid = .content_elt(content, .bundles_elt, "uuid"),
            bundleVersion = .content_elt(content, .bundles_elt, "version")

        ) %>%
        ## add hit and project index
        mutate(
            ## .data is the data frame being passed
            .hit = seq_along(.data$projectTitle),
            .project = lapply(lengths(.data$projectTitle), seq_len)
        ) %>%
        ## unnest list columns
        unnest(c(
            "projectTitle", "bundleUuid", "bundleVersion", "genusSpecies",
            "samples", "files", ".project"
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
#' @param as character(1) return format. Default: `"tibble"`, a tibble
#'     summarizing essential elements of HCA bundles. `"lol"`: a
#'     list-of-lists containing detailed file information.
#'
#' @seealso `lol_find()` and `lol_lfind()` for working with
#'     list-of-lists data structures.
#'
#' @return When `as = "tibble"`, `bundles()` returns a tibble with each
#'     row representing a file for one of the specified HCA project,
#'     and columns summarizing the file.  Each `.hit` is a single
#'     result; ....
#'
#'     When `as = "lol"`, `bundles()` returns a list-of-lists data
#'     structure representing detailed information on each file
#'     (`hit`).
#'
#' @export
#'
#' @examples
#' bundles(filters = filters(
#'     projectTitle = list(
#'         is = c("Tabula Muris: Transcriptomic characterization of 20 organs
#'         and tissues from Mus musculus at single cell resolution")
#'    )
#' ))
bundles <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol"))
{
    if (is.null(filters))
        filters <- filters()

    as <- match.arg(as) # defaults from argument

    response <- .index_GET(
        filters = filters,
        size = size,
        sort = sort,
        order = order,
        catalog = catalog,
        base_path = .BUNDLES_PATH
    )

    switch(
        as,
        tibble = .bundles_as_tibble(response$content),
        lol = response$content
    )
}
