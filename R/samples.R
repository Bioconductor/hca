.SAMPLES_PATH <- "/index/samples"

.SAMPLES_COLUMNS <- c(
    entryId = "hits[*].entryId",
    projectTitle = "hits[*].projects[*].projectTitle[*]",
    genusSpecies = "hits[*].donorOrganisms[*].genusSpecies[*]",
    samples.organ = "hits[*].samples[*].organ",
    disease = "hits[*].donorOrganisms[*].disease[*]",
    instrumentManufacturerModel =
        "hits[*].protocols[*].instrumentManufacturerModel[*]",
    fileType = "hits[*].fileTypeSummaries[*].fileType",
    count = "hits[*].fileTypeSummaries[*].count"
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
#' @examples
#' title <- paste(
#'     "Tabula Muris: Transcriptomic characterization of 20 organs and tissues from Mus musculus",
#'     "at single cell resolution"
#' )
#' filters <- filters( projectTitle = list(is = title) )
#' samples(filters = filters)
#'
#' @export
samples <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol", "list"),
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
        tibble = .as_tbl_hca(response$content, columns),
        lol = .as_lol_hca(response$content, columns),
        list = response$content
    )
}

#' @rdname samples
#'
#' @examples
#' samples_facets()
#'
#' @export
samples_facets <-
    function(
        facet = character(),
        catalog = c("dcp2", "it2", "dcp1", "it1")
    )
{
    stopifnot(
        is.character(facet), !anyNA(facet)
    )
    catalog <- match.arg(catalog)
    lst <- samples(size = 1L, catalog = catalog, as = "list")
    .term_facets(lst, facet)
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
