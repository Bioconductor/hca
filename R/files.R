.FILES_PATH <- "/index/files"

.FILES_DEFAULT_COLUMNS <- c(
    fileId = "hits[*].files[*].uuid",
    name = "hits[*].files[*].name",
    fileFormat = "hits[*].files[*].format",
    size = "hits[*].files[*].size",
    version = "hits[*].files[*].version",
    projectTitle = "hits[*].projects[*].projectTitle[*]",
    projectId = "hits[*].projects[*].projectId[*]",
    url = "hits[*].files[*].url"
)

.FILES_REQUIRED_COLUMNS <- c(
    fileId = "hits[*].files[*].uuid",
    name = "hits[*].files[*].name",
    fileFormat = "hits[*].files[*].format",
    version = "hits[*].files[*].version",
    projectTitle = "hits[*].projects[*].projectTitle[*]",
    projectId = "hits[*].projects[*].projectId[*]",
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
             catalog = NULL,
             as = c("tibble", "lol", "list"),
             columns = files_default_columns("character"))
{
    if (is.null(filters)){
        filters <- filters()
    }

    if(is.null(catalog)){
        catalog <- catalogs()[1]
    }


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
        tibble = .as_tbl_hca(response$content, columns, "files_tbl_hca"),
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
    # creating a BiocFileCache at the specified location
    bfc <- BiocFileCache(base_destination, ask = FALSE)
    # if file is not already in cache, proceed
    if (!NROW(bfcquery(bfc, file_id))) {
        # do not follow redirect
        config <-  httr::config(followlocation = 0)
        response <- httr::GET(ref_url, config = config)

        # location of file for download
        download_url <- headers(response)$location

        extension <- paste0(".", file_ext(file_name))

        savepath <- bfcnew(bfc, file_id, ext = extension)
        response <- GET(
            download_url, write_disk(savepath, overwrite = TRUE),
            # if running interactively, show file download progress bar
            if (interactive()) progress()
        )
    }
    # return the path to the file's location in the cache
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
#' for file downloads, or `NULL`
#'
#' @return `files_download()` returns a character() vector of file
#'     destinations
#' @importFrom dplyr %>% mutate filter
#' @importFrom tools R_user_dir
#'
#' @export
#'
#' @examples
#' files_filter <- filters(
#'     projectId = list(is = "cddab57b-6868-4be4-806f-395ed9dd635a"),
#'     fileFormat = list(is = "loom")
#' )
#' files_tbl <- files(filter = files_filter)
#' \dontrun{files_download(files_tbl, destination = tempdir())}
files_download <-
    function (tbl, destination = NULL)
{
    if (is.null(destination))
        ## this cache will persist across R sessions
        destination <- files_cache(create = TRUE)

    stopifnot(
        inherits(tbl, "files_tbl_hca"),
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
    lst <- files(size = 1L, catalog = catalog, as = "list")
    .term_facets(lst, facet)
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
#'     uuid = "ee6a75bd-3252-41ee-b253-425bbd377f0c")
#'
#' @export
files_detail <-
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
    .details(uuid = uuid, catalog = catalog, view = "files")
}

#' @rdname files
#'
#' @description `files_cache()` is the default location of the cache
#'     of downloaded files.
#'
#' @details `files_cache()` can be useful when it is necessary to
#'     'clean up' the cache, e.g., `BiocFileCache::cleanbfc()` or more
#'     dramatically `unlink(files_cache(), recursive = TRUE)`.
#'
#' @param create logical(1) create the default cache location, if it
#'     does not yet exist.
#'
#' @return `files_cache()` returns the path to the default cache. Use
#'     this as the `cache=` argument to `BiocFileCache()`.
#'
#' @export
#'
#' @examples
#' files_cache(create = FALSE)
files_cache <-
    function(create = FALSE)
{
    stopifnot(.is_scalar_logical(create))

    cache <- R_user_dir(package = "hca", which = "cache")
    if (create && !dir.exists(cache))
        dir.create(cache, recursive = TRUE)

    cache
}
