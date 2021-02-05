.COLUMNS_DEFAULTS <- list(
    projects = .PROJECTS_COLUMNS,
    files = .FILES_COLUMNS,
    samples = .SAMPLES_COLUMNS,
    bundles = .BUNDLES_COLUMNS
)

#' Tibble Functions
#'
#' @rdname tibbles
#'
#' @name next_tbl
#'
#' @param curr_tbl tibble current tibble for which we want the next page
#'
#' @return tibble
#'
#' @export
next_tbl <- function (curr_tbl)
    UseMethod("next_tbl")

#' @importFrom tibble as_tibble
#' @export
next_tbl.tbl_hca <- function (curr_tbl) {
    url <- attr(curr_tbl, "pagination")[["next"]]
    if (is.null(url)) {
        print("You are already on the last page of results.")
        return(url)
    } else {
        response <- GET(url)
        stop_for_status(response)
        result <- list(
            content = content(response),
            status_code = response$status_code
        )

        return(as_tibble(result))
    }
}

#' @rdname tibbles
#'
#' @name prev_tbl
#'
#' @param curr_tbl tibble current tibble for which we want the previous page
#'
#' @return tibble
#'
#' @export
prev_tbl <- function (curr_tbl)
    UseMethod("prev_tbl")

#' @importFrom tibble as_tibble
#' @export
prev_tbl.tbl_hca <- function (curr_tbl) {
    url <- attr(curr_tbl, "pagination")[["previous"]]
    if (is.null(url)) {
        print("You are already on the first page of results.")
        return(url)
    } else {
        response <- GET(url)
        stop_for_status(response)
        result <- list(
            content = content(response),
            status_code = response$status_code
        )

        return(as_tibble(result))
    }
}

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
.as_hca_tibble <-
    function(x, keys)
{
    if (is.null(names(keys))) {
        names(keys) <- keys
    } else {
        idx <- !nzchar(names(keys))
        names(keys)[idx] <- keys[idx]
    }
    values <- lapply(keys, lol_hits, x = x)
    hca_tbl <- as_tibble(values)

    attr(hca_tbl, "columns") <- keys
    attr(hca_tbl, "pagination") <- x$pagination

    class(hca_tbl) <- c("tbl_hca", class(tibble()))

    hca_tbl
}
