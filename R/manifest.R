.MANIFEST_PATH <- "/fetch/manifest/files"

.MANIFEST_DEFAULT_COLUMNS <- c()

.MANIFEST_REQUIRED_COLUMNS <- c()

#' @rdname manifest
#'
#' @name manifest
#'
#' @title HCA File Querying
#'
#' @description `manifest()` takes a list of user provided project titles
#' to be used to query the HCA API for information about
#' available manifest files.
NULL # don't add next function to documentation


#' @inheritParams projects
#'
#' @examples
#' title <- paste(
#'     "Tabula Muris: Transcriptomic characterization of 20 organs and tissues from Mus musculus",
#'     "at single cell resolution"
#' )
#' filters <- filters( projectTitle = list(is = title) )
#' manifest(filters = filters)
#'
#' @export
manifest <-
    function(filters = NULL,
             size = 1000L,
             sort = "projectTitle",
             order = c("asc", "desc"),
             catalog = NULL,
             as = c("tibble", "lol", "list"),
             columns = manifest_default_columns("character"))
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
            base_path = .MANIFEST_PATH
        )

        switch(
            as,
            tibble = .as_tbl_hca(response$content, columns, "manifest_tbl_hca"),
            lol = .as_lol_hca(response$content, columns),
            list = response$content
        )
    }

#' @rdname manifest
#'
#' @export
manifest_default_columns <-
    function(as = c("tibble", "character"))
    {
        .default_columns("manifest", as)
    }

## helper function for downloading manifest
## very similar to files:::.single_file_download
#' @importFrom httr progress GET stop_for_status content write_disk
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcnew bfcrpath
#' @importFrom tools file_ext
.single_manifest_download <- function(manifest_ref_url, base_destination) {
    # creating a BiocFileCache at the specified location
    bfc <- BiocFileCache(base_destination, ask = FALSE)
    # if file is not already in cache, proceed
    # using reference url as unique identifier
    if (!NROW(bfcquery(bfc, manifest_ref_url))) {
        # do not follow redirect
        config <-  httr::config(followlocation = 0)
        response <- httr::GET(manifest_ref_url, config = config)

        # location of manifest file for download
        download_url <- httr::content(response)$Location

        extension <- ".tsv"

        savepath <- bfcnew(bfc, manifest_ref_url, ext = extension)
        response <- GET(
            download_url, write_disk(savepath, overwrite = TRUE),
            # if running interactively, show file download progress bar
            if (interactive()) progress()
        )
    }
    # return the path to the manifest file's location in the cache
    unname(bfcrpath(bfc, manifest_ref_url))
}

#' @rdname manifest
#'
#' @description `manifest_download()` takes a tibble of manifest files
#' and a directory location as arguments to download the manifest of the tibble
#' into the specified directory. Limited to returning the first 1000 entries
#' that match the provided manifest_filter criteria.
#'
#' @param tbl tibble of manifest (result of `manifest()`)
#'
#' @param destination character() vector name of temporary directory to use
#' for file downloads, or `NULL`
#'
#' @return `manifest_download()` returns a character() vector of file
#'     destinations
#' @importFrom dplyr %>% mutate filter
#' @importFrom tools R_user_dir
#'
#' @export
#'
#' @examples
#' manifest_filter <- filters(
#'     projectId = list(is = "cddab57b-6868-4be4-806f-395ed9dd635a"),
#'     fileFormat = list(is = "loom")
#' )
#' \dontrun{manifest_download(manifest_filter, destination = tempdir())}
manifest_download <- function (manifest_filter, destination = NULL) {
    if (is.null(destination))
        ## this cache will persist across R sessions
        destination <- manifest_cache(create = TRUE)

    stopifnot(
        inherits(manifest_filter, "filters"),
        inherits(manifest_filter, "hca")
    )

    manifest_uri <- manifest_url_generator(filters = manifest_filter)
    manifest_file_location <- .single_manifest_download(
                                    manifest_uri,
                                    destination)
    manifest_file_location
}


#' @rdname manifest
#'
#' @examples
#' manifest_facets()
#' manifest_facets("fileFormat")
#'
#' @export
manifest_facets <-
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
        lst <- manifest(size = 1L, catalog = catalog, as = "list")
        .term_facets(lst, facet)
    }

#' @rdname manifest
#'
#' @name manifest_detail
#'
#' @description `manifest_detail()` takes a unique file_id and catalog for
#' the file, and returns details about the specified file as a
#' list-of-lists
#'
#' @return `manifest_detail()` returns a list-of-lists containing
#'     relevant details about the file.
#'
#' @examples
#' manifest_detail(
#'     uuid = "ee6a75bd-3252-41ee-b253-425bbd377f0c")
#'
#' @export
manifest_detail <-
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
        .details(uuid = uuid, catalog = catalog, view = "manifest")
    }

#' @rdname manifest
#'
#' @description `manifest_cache()` is the default location of the cache
#'     of downloaded manifest.
#'
#' @details `manifest_cache()` can be useful when it is necessary to
#'     'clean up' the cache, e.g., `BiocFileCache::cleanbfc()` or more
#'     dramatically `unlink(manifest_cache(), recursive = TRUE)`.
#'
#' @param create logical(1) create the default cache location, if it
#'     does not yet exist.
#'
#' @return `manifest_cache()` returns the path to the default cache. Use
#'     this as the `cache=` argument to `BiocFileCache()`.
#'
#' @export
#'
#' @examples
#' manifest_cache(create = FALSE)
manifest_cache <- function(create = FALSE)
{
    stopifnot(.is_scalar_logical(create))

    cache <- R_user_dir(package = "hca", which = "cache")
    if (create && !dir.exists(cache))
        dir.create(cache, recursive = TRUE)

    cache
}
