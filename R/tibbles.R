.TIBBLE_COLUMNS <- list(
    projects = .PROJECTS_COLUMNS,
    files = .FILES_COLUMNS,
    samples = .SAMPLES_COLUMNS,
    bundles = .BUNDLES_COLUMNS
)

#' @rdname projects
#'
#' @description `tibble_default_columns()` returns a tibble describing
#'     the content of the tibble returned by `projects()`, `files()`,
#'     `samples()`, and `bundles().
#'
#' @param view character(1) one of `"projects"`, `"files"`,
#'     `"samples"`, or `"bundles"`, corresponding to the name of the
#'     function for which default values are desired.
#'
#' @return `tibble_default_columns()` returns a tibble with column
#'     `name` containing the column name used in the tibble returned
#'     by `projects()`, `files()`, `samples()`, or `bundles()`, and
#'     `path` the path (see `lol_hits()`) to the data in the
#'     list-of-lists by the same functions when `as = "lol"`.
#'
#' @examples
#' tibble_default_columns("projects")
#'
#' @importFrom tibble tibble
#'
#' @export
tibble_default_columns <-
    function(
        view = c("projects", "files", "samples", "bundles"),
        as = c("tibble", "character")
    )
{
    view <- match.arg(view)
    as <- match.arg(as)

    keys <- .TIBBLE_COLUMNS[[view]]
    if (identical(as, "tibble")) {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- keys[idx]
        tibble(name = names(keys), path = unname(keys))
    } else {
        keys
    }
}

#' @importFrom tibble as_tibble
.as_tibble <-
    function(x, keys)
{
    if (is.null(names(keys))) {
        names(keys) <- keys
    } else {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- keys[idx]
    }
    values <- lapply(keys, lol_hits, x = x)
    as_tibble(values)
}
