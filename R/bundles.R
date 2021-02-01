.BUNDLES_PATH <- "/index/bundles"

.BUNDLES_COLUMNS <- c(
    projectTitle = "projects.projectTitle",
    genusSpecies = "donorOrganisms.genusSpecies",
    samples = "samples.id",
    files = "files.name",
    bundleUuid = "bundles.bundleUuid",
    bundleVersion = "bundles.bundleVersion"
)

#' @rdname bundles
#'
#' @name bundles
#'
#' @title HCA File Querying
#'
#' @description `bundles()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available bundles.
NULL # don't add next function to documentation

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
#' @param columns named character() indicating the paths to be used
#'     for parsing the 'lol' returned from the HCA to a tibble. The
#'     names of `columns` are used as column names in the returned
#'     tibble.
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
             as = c("tibble", "lol"),
             columns = bundles_default_columns("character"))
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
        tibble = .as_tibble(response$content, columns),
        lol = response$content
    )
}

#' @rdname bundles
#'
#' @examples
#' bundles_terms()
#'
#' export
bundles_terms <-
    function(
        facet = character(),
        catalog = c("dcp2", "it2", "dcp1", "it1")
    )
{
    stopifnot(
        is.character(facet), !anyNA(facet)
    )
    catalog <- match.arg(catalog)
    lol <- bundles(size = 1L, catalog = catalog, as = "lol")
    .term_facets(lol, facet)
}

#' @export
bundles_default_columns <-
    function(as = c("tibble", "character"))
{
    .default_columns("bundles", as)
}
