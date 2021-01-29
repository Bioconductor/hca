.FILES_PATH <- "/index/files"

#' @rdname files
#'
#' @name files
#'
#' @title HCA File Querying
#'
#' @description `files()` takes a list of user provided project titles
#'     to be used to query the HCA API for information about available files.

NULL # don't add next function to documentation

#' @importFrom tidyr unnest
#'
#' @importFrom dplyr %>% mutate filter rowwise
#'
#' @importFrom tibble tibble
.files_as_tibble <- function(content) {
    tibble(
        fileId = lol_hits(content, "files.uuid"),
        name = lol_hits(content, "files.name"),
        size = lol_hits(content, "files.size"),
        version = lol_hits(content, "files.version"),
        libraryConstructionApproach =
            lol_hits(content, "libraryConstructionApproach"),
        projectTitle = lol_hits(content, "projects.projectTitle"),
        url = lol_hits(content, "url")
    )
}

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
             as = c("tibble", "lol"))
{
    if (is.null(filters))
        filters <- filters()
    as <- match.arg(as)

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
        tibble = .files_as_tibble(response$content),
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
#' @name download_files
#'
#' @description `download_files()` takes a tibble of files and a directory
#' location as arguments to download the files of the tibble into the specified
#' directory.
#'
#' @param files_tib tibble of files (result of `files()`)
#'
#' @param destination
#'
#'
#' @return file_destinations vector of file destinations
#' @importFrom dplyr %>% mutate filter rowwise
#'
#' @export
#'
#' @examples
#' download_files(file_tib = files(filter = filters(
#' projectId = list(is = projectId),
#' fileFormat = list(is = "loom"))), destination = tempdir())
download_files <- function (files_tib = files(), destination = tempdir()) {
    # I couldn't quite figure out a smoother way to apply
    # a function rowwise in R
    file_destinations <- c()
    for(i in 1:nrow(files_tib)){
        new_dest <- .single_file_download(files_tib$url[[i]],
                                          files_tib$name[[i]],
                                          destination)
        file_destinations <- append(file_destinations, new_dest)
    }
    print(file_destinations)
    file_destinations
}