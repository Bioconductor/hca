.MANIFEST_FILE_PATH <- "/fetch/manifest/files"

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

#' @rdname manifest
#'
#' @description `manifest_url_generator()` takes a filter object with criteria
#' for the query and the catalog to search within.
#'
#' @param filter hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @return `manifest_url_generator()` returns a url string to be used in an
#' API call
#'
#' @export
#'
#' @examples
#' manifest_url_generator()
manifest_url_generator <- function(filters, catalog){

    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog),
        ## filter must be a valid filter
        `use 'filters()' to create 'filter=' argument` =
            inherits(filters, "filters")
    )

    manifest_index_path <- hca:::.index_path(
        filters = hca:::.filters_encoding(filters),
        catalog = catalog,
        base_path = .MANIFEST_FILE_PATH)

    manifest_uri_construct <- hca:::.hca_path(manifest_index_path)
    manifest_uri_construct
}

#' @rdname manifest
#'
#' @description `manifest_uuid_constructo()` takes a filter object with criteria
#' for the query and the catalog to search within.
#'
#' @param filter hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @return `manifest_uuid_constructo()` returns a url string to be used in an
#' API call
#'
#' @importFrom jsonlite fromJSON
#' @importFrom rlang squash
#'
#' @export
#'
#' @examples
#' manifest_url_generator(filters(), catalogs()[1])
manifest_uuid_constructor <- function(filters, catalog) {
    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog),
        ## filter must be a valid filter
        `use 'filters()' to create 'filter=' argument` =
            inherits(filters, "filters")
    )

    manifest_json <- hca:::.filters_json(manifest_filter)
    manifest_list <- jsonlite::fromJSON(manifest_json)
    manifest_list["catalog"] <- catalog
    ## help from: https://stackoverflow.com/questions/50915066/names-of-leaves-of-nested-list-in-r
    ## help from: https://stackoverflow.com/questions/22377713/concatenating-the-list-elements-in-r
    manifest_list_condensed_values <- suppressWarnings(sapply(rlang::squash(manifest_list),
                                                              paste,
                                                              collapse = ","))
    manifest_uuid <- paste(
        names(manifest_list),
        manifest_list_condensed_values,
        sep = "=",
        collapse = "&"
    )
}

## helper function for downloading manifest
## very similar to files:::.single_file_download
#' @importFrom httr progress GET stop_for_status content write_disk config
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcnew bfcrpath
#' @importFrom tools file_ext
.single_manifest_download <- function(manifest_id,
                                      manifest_ref_url,
                                      base_destination)
{
    # creating a BiocFileCache at the specified location
    bfc <- BiocFileCache(base_destination, ask = FALSE)
    # if file is not already in cache, proceed
    # using url as unique identifier
    if (!NROW(bfcquery(bfc, manifest_id))) {
        # do not follow redirect
        config <-  httr::config(followlocation = 0)
        ref_response <- httr::GET(manifest_ref_url, config = config)

        # location of file for download
        download_url <- httr::content(ref_response)$Location

        extension <- ".tsv"

        savepath <- bfcnew(bfc, manifest_id, ext = extension)
        manifest_response <- GET(
            download_url, write_disk(savepath, overwrite = TRUE),
            # if running interactively, show file download progress bar
            if (interactive()) progress()
        )
    }
    # return the path to the file's location in the cache
    unname(bfcrpath(bfc, manifest_id))
}

#' @param filter hca filter object
#'
#' @param catalog character() name of catalog
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#'
#' @examples
#' manifest_filter <- hca::filters(
#'     projectId = list(is = "4a95101c-9ffc-4f30-a809-f04518a23803"),
#'     fileFormat = list(is = "loom"),
#'     workflow = list(is = c("optimus_v4.2.2", "optimus_v4.2.3"))
#'     )
#' \dontrun{manifest(manifest_filter)}
#' @export
manifest <- function(filters = NULL, catalog = NULL) {

    if (is.null(filters)){
        filters <- filters()
    }

    if (is.null(catalog)){
        catalog <- catalogs()[1]
    }
    manifest_uri <- manifest_url_generator(filters = filters,
                                           catalog = catalog)
    manifest_uuid <- manifest_uuid_constructor(filters = filters,
                                               catalog = catalog)

    manifest_file_location <- .single_manifest_download(manifest_id = manifest_uuid,
                                                        manifest_ref_url = manifest_uri,
                                                        base_destination = manifest_cache(create = TRUE))

    manifest_tibble <- read.csv(manifest_file_location, sep = "\t") %>%
        tibble::as_tibble()
    ##switch(
        ##as,
        ##tibble = .as_tbl_hca(response$content, columns, "manifest_tbl_hca"),
        ##lol = .as_lol_hca(response$content, columns),
        ##list = response$content
    ##)
}

#' @rdname manifest
#'
#' @export
manifest_default_columns <- function(as = c("tibble", "character")) {
    .default_columns("manifest", as)
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
manifest_cache <- function(create = FALSE) {
    stopifnot(.is_scalar_logical(create))

    cache <- R_user_dir(package = "hca", which = "cache")
    if (create && !dir.exists(cache))
        dir.create(cache, recursive = TRUE)

    cache
}
