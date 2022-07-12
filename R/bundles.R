.BUNDLES_PATH <- "/index/bundles"

.BUNDLES_DEFAULT_COLUMNS <- c(
    projectTitle = "hits[*].projects[*].projectTitle[*]",
    genusSpecies = "hits[*].donorOrganisms[*].genusSpecies[*]",
    samples = "hits[*].samples[*].id[*]",
    files = "hits[*].files[*].name",
    bundleUuid = "hits[*].bundles[*].bundleUuid",
    bundleVersion = "hits[*].bundles[*].bundleVersion"
)

.BUNDLES_REQUIRED_COLUMNS <- c(
    bundleUuid = "hits[*].bundles[*].bundleUuid"
)

#' @rdname bundles
#'
#' @name bundles
#'
#' @title HCA Bundle Querying
#'
#' @description `bundles()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available bundles.
NULL # don't add next function to documentation

#' @inheritParams projects
#'
#' @examples
#' title <- paste(
#'     "Tabula Muris: Transcriptomic characterization of 20 organs and",
#'     "tissues from Mus musculus at single cell resolution"
#' )
#' filters <- filters( projectTitle = list(is = title) )
#' bundles(filters = filters)
#'
#' @export
bundles <-
    function(filters = NULL,
             size = 100L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = NULL,
             as = c("tibble", "lol", "list", "tibble_expanded"),
             columns = bundles_default_columns("character"))
{
    if (is.null(catalog)) {
        catalog <- catalogs()[1]
    }

    if (is.null(filters)){
        filters <- filters()
    }

    as <- match.arg(as) # defaults from argument

    stopifnot(
        size > 0L,
        .is_character(columns)
    )

    if (size > 100L && identical(as, "lol"))
        stop("'size' must be <= 100 when 'as = \"lol\"'")

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
        tibble = .as_tbl_hca(response$content, columns, "bundles_tbl_hca"),
        lol = .as_lol_hca(response$content, columns),
        list = response$content,
        tibble_expanded = .as_expanded_tbl_hca(
            response$content,
            exclude_path_pattern = character(),
            required_columns = .BUNDLES_REQUIRED_COLUMNS,
            type = "bundles_tbl_hca"
        )
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
        catalog = NULL
    )
{
    if(is.null(catalog)){
        catalog <- catalogs()[1]
    }

    stopifnot(
        is.character(facet),
        !anyNA(facet),
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog)
    )
    lst <- bundles(size = 1L, catalog = catalog, as = "list")
    .term_facets(lst, facet)
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
#' bundle <- bundles(size = 1, as = "list")
#' bundle_uuid <- bundle[["hits"]][[1]][["entryId"]]
#' bundles_detail(uuid = bundle_uuid) |> lol() |>
#' lol_filter(is_leaf) |> print(n = Inf)
#'
#' @export
bundles_detail <-
    function (uuid, catalog = NULL)
{
    if(is.null(catalog)){
        catalog <- catalogs()[1]
    }

    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog)
    )
    .details(uuid = uuid, catalog = catalog, view = "bundles")
}
