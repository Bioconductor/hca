.COLUMNS_DEFAULTS <- list(
    projects = .PROJECTS_DEFAULT_COLUMNS,
    files = .FILES_DEFAULT_COLUMNS,
    samples = .SAMPLES_DEFAULT_COLUMNS,
    bundles = .BUNDLES_DEFAULT_COLUMNS,
    test = c("hits[*].foo", "hits[*].bar")
)
.TEST_REQUIRED_COLUMNS <- c()

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

.tbl_hca_column_check <-
    function(input_tbl, default_columns, new_class)
{
    if (!all(names(default_columns) %in% names(input_tbl))) {
        ## missing column message
        error_message  <- paste0(
            "At minimum, these columns must be included in your tibble:\n    ",
            paste(
                sprintf('%s = \"%s\"', names(default_columns), default_columns),
                collapse = ",\n    "
            )
        )
        stop(error_message)
    }

    class(input_tbl) <- c(new_class, class(input_tbl))
    input_tbl
}

#' @importFrom tibble as_tibble
.as_tbl_hca <- function(x, keys, type)
{
    if (is.null(names(keys))) {
        keys <- .tbl_hca_name_columns(keys)
    } else {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- names(.tbl_hca_name_columns(keys[idx]))
    }
    lol <- .as_lol_hca(x, keys)

    idx <- keys %in% lol_path(lol)$path
    if (!any(idx)) {
        values <- rep(list(character()), length(keys))
        names(values) <- names(keys)
    } else {
        values <- lapply(keys[idx], lol_hits_pull, x = lol)
    }

    tbl_hca <- as_tibble(values)
    attr(tbl_hca, "keys") <- keys
    attr(tbl_hca, "pagination") <- x$pagination
    class(tbl_hca) <- c("tbl_hca", class(tbl_hca))

    required_columns <- switch(
        type,
        # check that they have at least the minimum necessary columns
        projects_tbl_hca = .PROJECTS_REQUIRED_COLUMNS,
        files_tbl_hca = .FILES_REQUIRED_COLUMNS,
        samples_tbl_hca = .SAMPLES_REQUIRED_COLUMNS,
        bundles_tbl_hca = .BUNDLES_REQUIRED_COLUMNS,
        test_tbl_hca = .TEST_REQUIRED_COLUMNS
    )

    .tbl_hca_column_check(tbl_hca, required_columns, type)
}

#' @importFrom dplyr pull
.as_expanded_tbl_hca <-
    function(x, exclude_path_pattern, required_columns, type)
{
    lol <- .as_lol_hca(x, character())
    path <-
        lol_path(lol) |>
        filter(.data$is_leaf) |>
        pull("path")
    if (length(exclude_path_pattern))
        path <- path[!grepl(exclude_path_pattern, path, ignore.case = TRUE)]
    column_keys <- .tbl_hca_name_columns(path)
    keys <- .tbl_hca_add_required_columns(column_keys, required_columns)
    .as_tbl_hca(x, keys, type)
}

## accessors

.tbl_hca_keys <- function(x)
    attr(x, "keys")

.tbl_hca_pagination <- function(x)
    attr(x, "pagination")

#' @rdname tbl_hca
#' @md
#'
#' @title 'tibble' representation of HCA query results
#'
#' @description `projects()`, `samples()`, `files()`, and `bundles()`
#'     return, by default, a 'tibble' representation of the query.
#'
#' `hca_next()` returns the next 'page' of results, if available.
#'
#' @param x a 'tibble' returned by `projects()`, `samples()`,
#'     `files()`, or `bundles()`.
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
    .as_tbl_hca(response$content, keys, class(x)[1])
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
    .as_tbl_hca(response$content, keys, class(x)[1])
}

## utilities

## clean up column queries to more user-friendly names
.tbl_hca_name_columns <-
    function(x)
{
    ## clean
    names1 <- sub("hits[*].", "", x, fixed = TRUE)
    names2 <- gsub("[*]", "", names1, fixed = TRUE)

    ## don't clean if results in duplicates
    count <- table(names2)
    idx <- names2 %in% names(count)[count > 1]
    names2[idx] <- names1[idx]

    ## apply updated names
    names(x) <- names2

    x
}

## ensure required columns are present & first
.tbl_hca_add_required_columns <-
    function(x, required_columns)
{
    ## add required columns at front
    x <- c(required_columns, x)
    ## eliminate duplicate entries
    x[!duplicated(x)]
}
