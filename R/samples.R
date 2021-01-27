.SAMPLES_PATH <- "/index/samples"

#' @rdname samples
#'
#' @name samples
#'
#' @title HCA File Querying
#'
#' @description `samples()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available samples.

NULL # don't add next function to documentation

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate
#'
#' @importFrom tibble tibble
.samples_as_tibble <- function(content) {
    tibble(
        entryId = lol_hits(content, "entryId"),
        projectTitle = lol_hits(content, "projects.projectTitle"),
        genusSpecies = lol_hits(content, "donorOrganisms.genusSpecies"),
        samples.organ = lol_hits(content, "samples.organ"),
        donorOrganisms.disease = lol_hits(content, "donorOrganisms.disease"),
        instrumentManufacturerModel =
            lol_hits(content, "instrumentManufacturerModel"),
        fileTypeSummaries.fileType =
            lol_hits(content, "fileTypeSummaries.fileType"),
        fileTypeSummaries.count = lol_hits(content, "fileTypeSummaries.count")
    )
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
#'     summarizing essential elements of HCA samples. `"lol"`: a
#'     list-of-lists containing detailed file information.
#'
#' @seealso `lol_find()` and `lol_lfind()` for working with
#'     list-of-lists data structures.
#'
#' @return When `as = "tibble"`, `samples()` returns a tibble with each
#'     row representing a file for one of the specified HCA project,
#'     and columns summarizing the file.  Each `.hit` is a single
#'     result; ....
#'
#'     When `as = "lol"`, `samples()` returns a list-of-lists data
#'     structure representing detailed information on each file
#'     (`hit`).
#'
#' @export
#'
#' @examples
#' samples(filters = filters(
#'     projectTitle = list(
#'         is = c("Tabula Muris: Transcriptomic characterization of 20 organs
#'         and tissues from Mus musculus at single cell resolution")
#'    )
#' ))
samples <-
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
        base_path = .SAMPLES_PATH
    )

    switch(
        as,
        tibble = .samples_as_tibble(response$content),
        lol = response$content
    )
}
