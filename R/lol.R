#' @rdname lol
#'
#' @name list-of-lists
#'
#' @aliases .lol_visit .lol_visit.list .lol_visit.default
#'
#' @title Utilities for manipulating lists-of-lists
NULL

## visit each node, searching for nodes named 'key'. When key found
## evaluate FUN(). Ignore nodes below any in 'not_in'. Return a
## list-of-lists reflecting the structure of the found nodes.

#' @export
.lol_visit <- function(x, FUN, key, not_in)
    UseMethod(".lol_visit")

#' @importFrom stats setNames
#' @importFrom utils head tail
#'
#' @export
.lol_visit.list <-
    function(x, FUN, key, not_in)
{
    ## list nodes; search for key or recurse into list elements
    if (is.null(names(x))) {
        found <- logical(length(x))
        result <- vector("list", length(x))
    } else {
        x <- x[!names(x) %in% not_in] # remove 'not_in' nodes from named lists
        found <- names(x) %in% key
        result <- setNames(vector("list", length(x)), names(x))
    }

    result[found] <- FUN(x[found])
    result[!found] <- lapply(x[!found], .lol_visit, FUN, key, not_in)

    result[found | lengths(result) > 0L]
}

#' @export
.lol_visit.default <-
    function(x, FUN, key, not_in)
{
    ## leaf nodes
    if (any(names(x) %in% key)) {
        return(FUN(x[key]))
    } else {
        return(FUN(NULL))
    }
}

#' @rdname lol
#'
#' @name list-of-lists
#'
#' @description `lol_find()` searches a list-of-lists `x` for nodes
#'     with name `key`, returning a named vector of scalar-valued
#'     keys.
#'
#' @param x list(), possibly containing other lists.
#'
#' @param key character() desired node name, or word-sufffix of path
#'     to nodes of interest, e.g., `"a.b"` finds "b" nodes that are
#'     children of "a" nodes.
#'
#' @param not_in character() of node(s) whose descendants are excluded
#'     from matching `key`.
#'
#' @param filter character(1) regular expression matching paths to be
#'     returned. Must be NA if `simplify` is FALSE.
#'
#' @return `lol_find()` returns a named character vector. The names
#'     represent the paths to each node matching `key`, and the
#'     elements correspond to the value of the node. Named keys with
#'     empty elements are represented as `NA`.
#'
#' @examples
#' lol <- list(a = list(b = 1), a = list(b = 2))
#' lol_find(lol, "b")
#' lol_count(lol, "b")
#'
#' lol <- list(list(b = 1), a = list(b = 2))
#' lol_find(lol, "b")
#' lol_find(lol, "b", not_in = "a")
#' str( lol_find(lol, "b") )
#'
#' ## empty key
#' lol_find(list(a = list(), a = list(1)), "a")  # c(NA, 1)
#'
#' ## 'a.b': choose 'b' nodes that are children of 'a' nodes
#' lol <- list(a = list(b = 1), c = list(b = 2))
#' lol_find(lol, "a.b")
#' lol_find(lol, "b", filter = "a.b")  # same as previous, but less robust
NULL

.lol_find_core <-
    function(x, keys, not_in)
{
    stopifnot(
        is.character(keys),
        is.character(not_in)
    )

    if (nzchar(keys))
        keys <- strsplit(keys, ".", fixed = TRUE)[[1]]

    x <- .lol_visit(x, identity, head(keys, 1), not_in)
    for (key in tail(keys, -1))
        x <- .lol_visit(x, identity, key, not_in)

    x
}

#' @rdname lol
#'
#' @export
lol_find <-
    function(x = list(), key = "", not_in = character(), filter = NA_character_)
{
    stopifnot(
        .is_scalar_character(filter, na.ok = TRUE)
    )

    result <- .lol_find_core(x, key, not_in)

    if (length(result)) {
        ## replace empty list elements with NA
        last_key <- tail(strsplit(key, ".", fixed = TRUE)[[1]], 1)
        FUN <- function(x) { x[lengths(x) == 0L] <- NA; x }
        result <- .lol_visit(result, FUN, last_key, character())
        ## unlist result
        result <- unlist(result)
    } else {
        result <- setNames(character(), character())
    }
    if (!is.na(filter))
        result <- result[grepl(filter, names(result))]

    result
}

#' @rdname lol
#'
#' @md
#'
#' @description `lol_count()` returns the number of nodes named `key`,
#'     following the same rules as `lol_find()`.
#'
#' @return `lol_count()` returns an integer(1) count of the number of
#'     elements matching `key`, subject to other arguments of the
#'     function.
#'
#' @examples
#' lol <- list(list(b = 1), a = list(b = 2), c = list(b = 3))
#' lol_count(lol, "a")
#' lol_count(lol, "b")
#' lol_count(lol, "b", not_in = "a")
#' lol_count(lol, "b", not_in = c("a", "c"))
#'
#' @export
lol_count <-
    function(x = list(), key = "", not_in = character(), filter = NA_character_)
{
    stopifnot(
        .is_scalar_character(key),
        is.character(not_in) && !anyNA(not_in),
        .is_scalar_character(filter, na.ok = TRUE)
    )

    result <- .lol_visit(x, function(x) sum(length(x)), key, not_in)

    result <- unlist(result)
    if (!is.na(filter))
        result <- result[grepl(filter, names(result))]

    sum(result)
}
