.MANIFEST_FILE_PATH <- "/fetch/manifest/files"

#' @rdname manifest
#'
#' @name manifest
#'
#' @title HCA File Querying
#'
#' @description `manifest()` takes a list of user provided project
#'     titles to be used to query the HCA API for information about
#'     available manifest files.
NULL # don't add next function to documentation

#' @rdname manifest-internal
#'
#' @title Internal functions used by `manifest()`
#'
#' @description `manifest_url_generator()` takes a filter object with
#'     criteria for the query and the catalog to search within.
#'
#' @param manifest_filter hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @return `manifest_url_generator()` returns a url string to be used
#'     in an API call
#'
#' @keywords internal
#'
#' @examples
#' manifest_url_generator(filters(), catalogs()[1])
manifest_url_generator <-
    function(manifest_filter, catalog)
{
    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog),
        ## manifest_filter must be a valid filter
        `use 'filters()' to create 'manifest_filter=' argument` =
            inherits(manifest_filter, "filters")
    )

    manifest_index_path <- .index_path(
        catalog = catalog,
        filters = .filters_encoding(manifest_filter),
        base_path = .MANIFEST_FILE_PATH)

    .hca_path(manifest_index_path)
}

#' @rdname manifest-internal
#'
#' @description `manifest_uuid_constructor()` takes a filter object
#'     with criteria for the query and the catalog to search within.
#'
#' @param manifest_filter hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @return `manifest_uuid_constructo()` returns a url string to be
#'     used in an API call
#'
#' @importFrom jsonlite fromJSON
#' @importFrom digest digest
#'
#' @keywords internal
#'
#' @examples
#' manifest_uuid_generator(filters(), catalogs()[1])
manifest_uuid_constructor <-
    function(manifest_filter, catalog)
{
    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog),
        ## filter must be a valid filter
        `use 'filters()' to create 'manifest_filter=' argument` =
            inherits(manifest_filter, "filters")
    )

    manifest_list <- manifest_filter$filters
    manifest_list["catalog"] <- catalog
    ## alphabetize the elements of the list to make order uniform
    ## manifest_filter$filters[indices]
    ## or hash for uuid: digest::digest(manifest_filter$filters)
    ## remember to make different indices for each filter
    ordered_indices <- order(names(manifest_list))
    manifest_uuid <- digest::digest(manifest_list[ordered_indices])
    manifest_uuid
}

## helper function for downloading manifest
## very similar to .single_file_download
#' @importFrom httr progress GET stop_for_status content write_disk config
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcnew bfcrpath
#' @importFrom tools file_ext
.single_manifest_download <-
    function(manifest_id, manifest_ref_url, base_destination, update_cache)
{
    ## creating a BiocFileCache at the specified location
    bfc <- BiocFileCache(base_destination, ask = FALSE)
    ## if file is not already in cache, use url as unique identifier
    bfc_query <- bfcquery(bfc, manifest_id)
    if (update_cache || !NROW(bfc_query)) {
        ## do not follow redirect
        config <-  httr::config(followlocation = 0)

        total_wait <- 0L
        message("requesting manifest")
        repeat {
            ref_response <- httr::GET(manifest_ref_url, config = config)
            stop_for_status(ref_response)

            content <- httr::content(ref_response)
            status <- content$Status
            if (identical(status, 302L))
                break
            else if (identical(status, 301L)) {
                wait <- content$`Retry-After`
                total_wait <- total_wait + wait
                manifest_ref_url <- content$Location
                message(
                    "waiting ", wait, " more seconds ",
                "(", total_wait, " seconds total)..."
                )
                Sys.sleep(wait)
            } else {
                stop(
                    "unexpected response generating manifest:",
                    "\n  Status: ", status
                )
            }
        }

        ## location of file for download
        download_url <- content$Location
        extension <- ".tsv"
        if (NROW(bfc_query)) {
            savepath <- bfc_query$rpath
        } else {
            savepath <- bfcnew(bfc, manifest_id, ext = extension)
        }
        manifest_response <- GET(
            download_url, write_disk(savepath, overwrite = TRUE),
            ## if running interactively, show file download progress bar
            if (interactive()) progress()
        )
        stop_for_status(manifest_response)
    }

    ## return the path to the file's location in the cache
    unname(bfcrpath(bfc, manifest_id))
}

#' @rdname manifest
#'
#' @param filters hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @param update_cache logical(1) when `TRUE`, update an existing
#'     cached resource by querying the HCA data server.
#'
#' @importFrom readr read_tsv
#' @importFrom tibble as_tibble
#'
#' @examples
#' manifest_filter <- hca::filters(
#'     projectId = list(is = "4a95101c-9ffc-4f30-a809-f04518a23803"),
#'     fileFormat = list(is = "loom"),
#'     workflow = list(is = c("optimus_v4.2.2", "optimus_v4.2.3"))
#' )
#' \dontrun{
#' result <- manifest(manifest_filter)
#' result
#' }
#' @export
manifest <-
    function(filters = NULL, catalog = NULL, update_cache = FALSE)
{

    if (is.null(filters)){
        filters <- filters()
    }

    if (is.null(catalog)){
        catalog <- catalogs()[1]
    }
    stopifnot(
        "`filters` must be NULL or the result of a call to `filters()`" =
            inherits(filters, "filters"),
        "`catalog` must be character(1) matching a value in `catalogs()`" =
            .is_scalar_character(catalog) && (catalog %in% catalogs()),
        .is_scalar_logical(update_cache)
    )

    manifest_uri <- manifest_url_generator(
        manifest_filter = filters,
        catalog = catalog
    )

    manifest_uuid <- manifest_uuid_constructor(
        manifest_filter = filters,
        catalog = catalog
    )

    manifest_file_location <- .single_manifest_download(
        manifest_id = manifest_uuid,
        manifest_ref_url = manifest_uri,
        base_destination = manifest_cache(create = TRUE),
        update_cache = update_cache
    )

    manifest_tibble <- read_tsv(manifest_file_location, show_col_types = FALSE)

    ## make returned tbl extend tbl_df not tbl_hca
    ## i.e. manifest_tbl_hca tbl_df ...
    class(manifest_tibble) <- c("manifest_tbl_hca", class(manifest_tibble))

    manifest_tibble
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
manifest_cache <-
    function(create = FALSE)
{
    stopifnot(.is_scalar_logical(create))

    cache <- R_user_dir(package = "hca", which = "cache")
    if (create && !dir.exists(cache))
        dir.create(cache, recursive = TRUE)

    cache
}
