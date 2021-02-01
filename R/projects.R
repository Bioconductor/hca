.PROJECTS_PATH <- "/index/projects"

.PROJECTS_COLUMNS <- c(
    projectId = "entryId",
    projectTitle = "projects.projectTitle",
    genusSpecies = "donorOrganisms.genusSpecies",
    "samples.organ",
    "specimens.organ"
)

#' @rdname projects
#'
#' @name projects
#'
#' @title HCA Project Querying
#'
#' @description `projects()` takes user input to be used to query the
#'     HCA API for information about available projects.
NULL # don't add next function to documentation

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate
#'
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
#'     summarizing essential elements of HCA projects. `"lol"`: a
#'     list-of-lists containing detailed project information
#'
#' @seealso `lol_find()` and `lol_lfind()` for working with
#'     list-of-lists data structures.
#'
#' @return When `as = "tibble"`, `projects()` returns a tibble with
#'     each row representing an HCA project, and columns summarizing
#'     the project. Each `.hit` is a single result; a result may
#'     contain several projects, as indexed by `.project`.
#'
#'     When `as = "lol"`, `projects()` returns a list-of-lists data
#'     structure representing detailed information on each project
#'     (`hit`).
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
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol"),
             columns = projects_default_columns("character"))
{
    if (is.null(filters))
        filters <- filters()

    as <- match.arg(as)

    response <- .index_GET(
        filters = filters,
        size = size,
        sort = sort,
        order = order,
        catalog = catalog,
        base_path = .PROJECTS_PATH
    )

    switch(
        as,
        tibble = .as_tibble(response$content, columns),
        lol = response$content
    )
}

#' @rdname projects
#'
#' @description `projects_terms()` summarizes facets and terms used by
#'     all records in the projects index.
#'
#' @param facet character() of valid facet names. Summary results (see
#'     'Value', below) are returned when missing or length greater
#'     than 1; details are returned when a single facet is specified.
#'
#' @return `projects_terms()` invoked with no `facet=` argument returns a
#'     tibble summarizing terms available as `projects()` return
#'     values, and for use in filters. The tibble contains columns
#'
#'     - `facet`: the name of the facet.
#'     - `n_terms`: the number of distinct values the facet can take.
#'     - `n_values`: the number of occurrences of the facet term in the
#'        entire catalog.
#'
#'     `projects_terms()` invoked with a scalar value for `facet=`
#'     returns a tibble summarizing terms used in the facet, and the
#'     number of occurrences of the term in the entire catalog.
#'
#' @examples
#' projects_terms()
#' projects_terms("genusSpecies")
#'
#' @export
projects_terms <-
    function(
        facet = character(),
        catalog = c("dcp2", "it2", "dcp1", "it1")
    )
{
    stopifnot(
        is.character(facet), !anyNA(facet)
    )
    catalog <- match.arg(catalog)
    lol <- projects(size = 1L, catalog = catalog, as = "lol")
    .term_facets(lol, facet)
}

#' @rdname projects
#'
#' @description `*_columns()` returns a tibble or named
#'     character vector describing the content of the tibble returned
#'     by `projects()`, `files()`, `samples()`, or `bundles().
#'
#' @return `*_columns()` returns a tibble with column `name`
#'     containing the column name used in the tibble returned by
#'     `projects()`, `files()`, `samples()`, or `bundles()`, and
#'     `path` the path (see `lol_hits()`) to the data in the
#'     list-of-lists by the same functions when `as = "lol"`. When `as
#'     = "character"`, the return value is a named list with paths as
#'     elements and abbreviations as names.
#'
#' @examples
#' projects_columns("projects")
#'
#' @export
projects_default_columns <-
    function(as = c("tibble", "character"))
{
    .default_columns("projects", as)
}
