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

#'
#' @inheritParams projects
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
#'         is = "Tabula Muris: Transcriptomic characterization of 20 organs and tissues from Mus musculus at single cell resolution"
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
        tibble = .as_hca_tibble(response$content, columns),
        lol = response$content
    )
}

#' @rdname bundles
#'
#' @examples
#' bundles_facets()
#'
#' @export
bundles_facets <-
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

#' @rdname bundles
#'
#' @export
bundles_default_columns <-
    function(as = c("tibble", "character"))
{
    .default_columns("bundles", as)
}

#' @rdname bundles
#'
#' @name bundles_detail
#'
#' @description `bundles_detail()` takes a unique bundle_id and catalog for
#' the bundle, and returns details about the specified bundle as a
#' list-of-lists
#'
#' @return `bundles_detail()` returns a list-of-lists containing
#'     relevant details about the bundle
#'
#' @examples
#' bundles_detail(
#'     uuid = "00aa6c53-71b5-4c12-98c4-54eb8173ffa5",
#'     catalog = "dcp2"
#' )
#'
#' @export
bundles_detail <-
    function (uuid, catalog = c("dcp2", "it2", "dcp1", "it1"))
{
    catalog <- match.arg(catalog)
    .details(uuid = uuid, catalog = catalog, view = "bundles")
}
