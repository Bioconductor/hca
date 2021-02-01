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
