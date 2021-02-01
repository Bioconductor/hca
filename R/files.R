.FILES_PATH <- "/index/files"

.FILES_COLUMNS <- c(
    fileId = "files.uuid",
    name = "files.name",
    size = "files.size",
    version = "files.version",
    projectTitle = "projects.projectTitle",
    url = "files.url"
)

#' @rdname files
#'
#' @name files
#'
#' @title HCA File Querying
#'
#' @description `files()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available files.
NULL # don't add next function to documentation

#' @param filters filter object created by `filters()`, or `NULL`
#'     (default; all files).
#'
#' @param size integer(1) maximum number of results to return;
#'     default: all files matching `filter`. The default (10000) is
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
#'     summarizing essential elements of HCA files. `"lol"`: a
#'     list-of-lists containing detailed file information.
#'
#' @seealso `lol_find()` and `lol_lfind()` for working with
#'     list-of-lists data structures.
#'
#' @return When `as = "tibble"`, `files()` returns a tibble with each
#'     row representing a file for one of the specified HCA project,
#'     and columns summarizing the file.  Each `.hit` is a single
#'     result; ....
#'
#'     When `as = "lol"`, `files()` returns a list-of-lists data
#'     structure representing detailed information on each file
#'     (`hit`).
#'
#' @export
#'
#' @examples
#' files(filters = filters(
#'     projectTitle = list(
#'         is = c("Tabula Muris: Transcriptomic characterization of 20 organs
#'         and tissues from Mus musculus at single cell resolution")
#'    )
#' ))
files <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol"),
             columns = tibble_default_columns("files", "character"))
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
        base_path = .FILES_PATH
    )

    switch(
        as,
        tibble = .as_tibble(response$content, columns),
        lol = response$content
    )
}
## helper function for downloading files
#' @importFrom httr progress GET stop_for_status content write_disk
.single_file_download <- function(url, name, base_destination) {
    response <- GET(url)
    stop_for_status(response)

    content <- content(response)
    content$Status # ?? how to use

    url <- content$Location
    destination <- file.path(base_destination, name)
    response <- GET(url, write_disk(destination, overwrite = TRUE), progress())
    stop_for_status(response)

    destination
}

#' @rdname files
#'
#' @description `files_download()` takes a tibble of files and a directory
#' location as arguments to download the files of the tibble into the specified
#' directory.
#'
#' @param tbl tibble of files (result of `files()`)
#'
#' @param destination character() vector name of temporary directory to use
#' for file downloads
#'
#'
#' @return file_destinations vector of file destinations
#' @importFrom dplyr %>% mutate filter
#'
#' @export
#'
#' @examples
#' files_filter <- filters(
#'     projectId = list(is = "cddab57b-6868-4be4-806f-395ed9dd635a"),
#'     fileFormat = list(is = "loom")
#' )
#' files_tbl <- files(filter = files_filter)
#' files_download(files_tbl, catalog = "dcp1", destination = tempdir())
files_download <-
    function (tbl, destination = tempdir())
{
    stopifnot(
        inherits(tbl, "data.frame"),
        `'tbl=' must contain columns "url", "name"` =
            all(c("url", "name") %in% names(tbl)),
        `'destination=' must be an existing directory` =
            .is_scalar_character(destination) && dir.exists(destination),
        `'destination=' must not contain files in tbl$name` =
            !any(file.exists(file.path(destination, tbl$name)))
    )

    mapply(
        .single_file_download,
        tbl$url, tbl$name,
        MoreArgs = list(base_destination = destination)
    )
}


#' @rdname files
#'
#' @examples
#' files_terms()
#' files_terms("fileFormat")
#'
#' @export
files_terms <-
    function(
        facet = character(),
        catalog = c("dcp2", "it2", "dcp1", "it1")
    )
{
    stopifnot(
        is.character(facet), !anyNA(facet)
    )
    catalog <- match.arg(catalog)
    lol <- files(size = 1L, catalog = catalog, as = "lol")
    .term_facets(lol, facet)
}
