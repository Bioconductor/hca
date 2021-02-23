.FILES_PATH <- "/index/files"

.FILES_COLUMNS <- c(
    fileId = "hits[*].files[*].uuid",
    name = "hits[*].files[*].name",
    size = "hits[*].files[*].size",
    version = "hits[*].files[*].version",
    projectTitle = "hits[*].projects[*].projectTitle[*]",
    url = "hits[*].files[*].url"
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


#' @inheritParams projects
#'
#' @examples
#' title <- paste(
#'     "Tabula Muris: Transcriptomic characterization of 20 organs and tissues from Mus musculus",
#'     "at single cell resolution"
#' )
#' filters <- filters( projectTitle = list(is = title) )
#' files(filters = filters)
#'
#' @export
files <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = c("dcp2", "it2", "dcp1", "it1"),
             as = c("tibble", "lol", "list"),
             columns = files_default_columns("character"))
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
        tibble = .as_tbl_hca(response$content, columns),
        lol = .as_lol_hca(response$content, columns),
        list = response$content
    )
}

#' @rdname files
#'
#' @export
files_default_columns <-
    function(as = c("tibble", "character"))
{
    .default_columns("files", as)
}

## helper function for downloading files
#' @importFrom httr progress GET stop_for_status content write_disk
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcnew bfcrpath
#' @importFrom tools file_ext
.single_file_download <-
    function(file_id, file_name, ref_url, base_destination)
{
    bfc <- BiocFileCache(base_destination, ask = FALSE)
    if (!NROW(bfcquery(bfc, file_id))) {
        response <- GET(ref_url)
        stop_for_status(response)

        content <- content(response)
        download_url <- content$Location

        extension <- paste0(".", file_ext(file_name))

        savepath <- bfcnew(bfc, file_id, ext = extension)
        response <- GET(
            download_url, write_disk(savepath, overwrite = TRUE),
            if (interactive()) progress()
        )
    }
    unname(bfcrpath(bfc, file_id))
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
#' files_tbl <- files(filter = files_filter, catalog = "dcp1")
#' \dontrun{files_download(files_tbl, destination = tempdir())}
files_download <-
    function (tbl, destination = tempdir())
{
    stopifnot(
        inherits(tbl, "tbl_hca"),
        `'tbl=' must contain columns "url", "name"` =
            all(c("fileId", "url", "name") %in% names(tbl)),
        `'destination=' must be an existing directory` =
            .is_scalar_character(destination) && dir.exists(destination),
        `'destination=' must not contain files in tbl$name` =
            !any(file.exists(file.path(destination, tbl$name)))
    )

    file_id <- paste(tbl$fileId, tbl$version, sep = "-")
    mapply(
        .single_file_download,
        file_id, tbl$name, tbl$url,
        MoreArgs = list(base_destination = destination)
    )
}


#' @rdname files
#'
#' @examples
#' files_facets()
#' files_facets("fileFormat")
#'
#' @export
files_facets <-
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

#' @rdname files
#'
#' @name files_detail
#'
#' @description `files_detail()` takes a unique file_id and catalog for
#' the file, and returns details about the specified file as a
#' list-of-lists
#'
#' @return `files_detail()` returns a list-of-lists containing
#'     relevant details about the file.
#'
#' @examples
#' files_detail(
#'     uuid = "bb4185da-c3da-4bb3-ab51-30aafbb60a0d",
#'     catalog = "dcp2"
#' )
#'
#' @export
files_detail <-
    function (uuid, catalog = c("dcp2", "it2", "dcp1", "it1"))
{
    catalog <- match.arg(catalog)
    .details(uuid = uuid, catalog = catalog, view = "files")
}
