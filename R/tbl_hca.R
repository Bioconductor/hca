.COLUMNS_DEFAULTS <- list(
    projects = .PROJECTS_COLUMNS,
    files = .FILES_COLUMNS,
    samples = .SAMPLES_COLUMNS,
    bundles = .BUNDLES_COLUMNS
)

#' @importFrom tibble tibble
.default_columns <-
    function(
        view = c("projects", "files", "samples", "bundles"),
        as = c("tibble", "character")
    )
{
    view <- match.arg(view)
    as <- match.arg(as)

    keys <- .COLUMNS_DEFAULTS[[view]]
    if (identical(as, "tibble")) {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- keys[idx]
        tibble(name = names(keys), path = unname(keys))
    } else {
        keys
    }
}

#' @importFrom tibble as_tibble
.as_tbl_hca <-
    function(x, keys)
{
    if (is.null(names(keys))) {
        names(keys) <- keys
    } else {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- keys[idx]
    }
    values <- lapply(keys, lol_hits, x = x)

    tbl_hca <- as_tibble(values)
    attr(tbl_hca, "keys") <- keys
    attr(tbl_hca, "pagination") <- x$pagination
    class(tbl_hca) <- c("tbl_hca", class(tbl_hca))

    tbl_hca
}

## accessors

.tbl_hca_keys <- function(x)
    attr(x, "keys")

.tbl_hca_pagination <- function(x)
    attr(x, "pagination")

#' @rdname tbl_hca
#' @md
#'
#' @title 'tibble' reprsentation of HCA query results
#'
#' @description `projects()`, `samples()`, `files()` and `bundles()`
#'     return, by default, a 'tibble' represenation of the query.
#'
#'     `hca_next()` returns the next 'page' of results, if available.
#'
#' @param x a 'tibble' returned by `projects()`, `samples()`,
#'     `files()`, or `bundles()`
#'
#' @return `hca_next()` returns a tibble, with the same columns as
#'     `x`, containing the next 'page' of results.
#'
#' @examples
#' projects <- projects(size = 5)      # projects 1-5
#' next_projects <- hca_next(projects) # projects 6-10
#'
#' @export
hca_next.tbl_hca <-
    function (x)
{
    pagination <- .tbl_hca_pagination(x)
    keys <- .tbl_hca_keys(x)
    response <- .hca_next(pagination)
    .as_tbl_hca(response$content, keys)
}

#' @rdname tbl_hca
#'
#' @description `hca_prev()` returns the previous 'page' of results.
#'
#' @return `hca_prev()` returns a tibble with the same columns as `x`,
#'     containing the previous 'page' of results.
#'
#' @examples
#' hca_prev(next_projects)            # projects 1-5
#'
#' @export
hca_prev.tbl_hca <-
    function (x)
{
    pagination <- .tbl_hca_pagination(x)
    keys <- .tbl_hca_keys(x)
    response <- .hca_prev(pagination)
    .as_tbl_hca(response$content, keys)
}
