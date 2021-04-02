## helper functions
## internal only

.ENDPOINTS <- list(
    projects = "/index/projects/",
    files = "/index/files/",
    samples = "/index/samples/",
    bundles = "/index/bundles/"
)

#' @rdname details
#'
#' @name .details
#'
#' @title Single Entity Details
#'
#' @param view character() type of entity i.e. project, file, sample, or bundle
#'
#' @param uuid character() unique *_id
#'
#' @param catalog character(1) source of data. Use `catalogs()`
#'    for possible values.
#'
#' @return list-of-lists containing relevant details about the project, file,
#' sample, or bundle
.details <-
    function(
        uuid = character(),
        catalog = NULL,
        view = c("projects", "files", "samples", "bundles")
    )
{
    if(is.null(catalog)){
        catalog <- catalogs()[1]
    }

    stopifnot(
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog)
    )
    view <- match.arg(view)

    base_path <- .ENDPOINTS[[view]]
    url_params <- list(
        catalog = catalog
    )

    parameters_path <- paste(
        names(url_params), unname(url_params), sep = "=", collapse = "&"
    )

    index_path <- paste0(base_path, uuid, "?", parameters_path)

    response <- .hca_GET(index_path)

    response$content

}