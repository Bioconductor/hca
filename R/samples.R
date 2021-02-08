.SAMPLES_PATH <- "/index/samples"

.SAMPLES_COLUMNS <- c(
    entryId = "entryId",
    projectTitle = "projects.projectTitle",
    genusSpecies = "donorOrganisms.genusSpecies",
    samples.organ = "samples.organ",
    disease = "donorOrganisms.disease",
    instrumentManufacturerModel = "instrumentManufacturerModel",
    fileType = "fileTypeSummaries.fileType",
    count = "fileTypeSummaries.count"
)

#' @rdname samples
#'
#' @name samples
#'
#' @title HCA File Querying
#'
#' @description `samples()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available samples.
NULL # don't add next function to documentation

#' @inheritParams projects
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
#' @examples
#' samples(filters = filters(
#'     projectTitle = list(
#'         is = c("Tabula Muris: Transcriptomic characterization of 20 organs
#'         and tissues from Mus musculus at single cell resolution")
#'    )
#' ))
#'
#' @export
samples <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol"),
             columns = samples_default_columns("character"))
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
        tibble = .as_hca_tibble(response$content, columns),
        lol = response$content
    )
}

#' @rdname samples
#'
#' @examples
#' samples_terms()
#'
#' @export
samples_terms <-
    function(
        facet = character(),
        catalog = c("dcp2", "it2", "dcp1", "it1")
    )
{
    stopifnot(
        is.character(facet), !anyNA(facet)
    )
    catalog <- match.arg(catalog)
    lol <- samples(size = 1L, catalog = catalog, as = "lol")
    .term_facets(lol, facet)
}

#' @rdname samples
#'
#' @export
samples_default_columns <-
    function(as = c("tibble", "character"))
{
    .default_columns("samples", as)
}

#' @rdname samples
#'
#' @name samples_detail
#'
#' @description `samples_detail()` takes a unique sample_id and catalog for
#' the sample, and returns details about the specified sample as a
#' list-of-lists
#'
#' @return `samples_detail()` returns a list-of-lists containing
#'     relevant details about the sample
#'
#' @examples
#' samples_detail(
#'     uuid = "1dda6a28-cbaa-4506-be47-fa117e8f463c",
#'     catalog = "dcp2"
#' )
#'
#' @export
samples_detail <-
    function (uuid, catalog = c("dcp2", "it2", "dcp1", "it1"))
{
    catalog <- match.arg(catalog)
    .details(uuid = uuid, catalog = catalog, view = "samples")
}
