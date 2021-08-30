#' @rdname loom_annotation
#'
#' @name loom_annotation
#'
#' @title HCA loom file annotation
#'
#' @description `loom_annotation()` takes the file path location of a .loom file
#' for which additional data will be extracted from the appropriate manifest.
#' The .loom file will be imported as a SummarizedExperiment object, and the
#' additional manifest information will be added to the object for return.

#' @importFrom dplyr %>% mutate
#' @importFrom dplyr %>% mutate
#'
#' @param loom_file_path character() file path of loom file on user's system.
#'
#' @seealso `manifest()` and related functions for working with data returned
#' from the `*/manifest/*` HCA API endpoints.
#'
#' @return SummarizedExperiment object
#'
#' @export
loom_annotation <- function(loom_file_path = NULL) {
    stopifnot(
        ## loom_file_path must be a character string
        `loom_file_path must be a non-null file path` =
            .is_scalar_character(loom_file_path),
        ## loom_file_path must be an existing file
        `loom_file_path must be an existing file` =
            file.exists(loom_file_path),
        ## loom_file_path must be a loom file
        `loom_file_path must be an existing file` =
            file_ext(loom_file_path) == 'loom'
    )

}