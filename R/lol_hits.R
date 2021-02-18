#' @rdname lol
#' @md
#'
#' @description `lol_hits_lpull()` and `lol_hits_pull()` are variants
#'     of `lol_lpull()` and `lol_pull()` that retain the original
#'     geometry of `hits[*]`, even when the mapping between `hits[*]`
#'     and `path` is not 1:1.
#'
#' @export
lol_hits_lpull <-
    function(x, path)
{
    stopifnot(
        inherits(x, "lol"),
        .is_scalar_character(path),
        .lol_valid_path(x, path)
    )

    within_dict <- .lol_dict(x)[["hits[*]"]]
    within_depth <- unique(lengths(within_dict))
    within_idx <- vapply(within_dict, `[[`, integer(1), within_depth)

    path_dict <- .lol_dict(x)[[path]]
    path_idx <- factor(
        vapply(path_dict, `[[`, integer(1), within_depth),
        levels = within_idx
    )

    value <- vector("list", length(within_idx))
    value[as.integer(levels(path_idx))] <-
        lol_lpull(x, path) |>
        split(path_idx) |>
        lapply(unname)
    value
}

#' @rdname lol
#' @md
#'
#' @export
lol_hits_pull <-
    function(x, path)
{
    template <- lol_hits_lpull(x, path)
    template <- lapply(template, unlist, recursive = FALSE)
    idx <- vapply(template, is.null, logical(1))

    if (all(lengths(template) < 2L)) {
        template <- unlist(template, use.names = FALSE)
        value <- vector(class(template), length(idx))
        value[idx] <- NA
        value[!idx] <- template[!idx]
    } else {
        uclass <- unique(vapply(template[!idx], class, character(1)))
        if (length(uclass) == 1L)
            template[idx] <- list(vector(uclass, 0))
        value <- template
    }

    value
}
