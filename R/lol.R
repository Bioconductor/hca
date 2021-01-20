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

    result[lengths(result) > 0L]
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
#' @md
#'
#' @description `lol_find()` searches a list-of-lists `x` for nodes
#'     with name `key`. The form and content of the output is
#'     determined by `simplify` and `filter`.
#'
#' @param x list(), possibly containing other lists.
#'
#' @param key character(1) desired node name.
#'
#' @param not_in character() of node(s) whose descendants are excluded
#'     from matching `key`.
#'
#' @param simplify logical(1) when TRUE, `unlist()` the result to
#'     return a named character() vector, where elements correspond
#'     node values matching `key`, and names describe the named path
#'     to the node. When FALSE, a list-of-lists reflecting the
#'     structure of the data leading to each `key`. See examples.
#'
#' @param filter character(1) regular expression matching paths to be
#'     returned. Must be NA if `simplify` is FALSE.
#'
#' @return `lol_find()` returns a named vector when `simplify =
#'     TRUE`. The names represent the paths to each node matching
#'     `key`, and the elements correspond to the value of the node.
#'
#'     `lol_find()` returns a list-of-lists when `simplify =
#'     FALSE`. The list-of-lists reflects the list structure of `x`,
#'     but trimmed to only include paths leading to `key`.
#'
#' @examples
#' lol <- list(a = list(b = 1), a = list(b = 2))
#' lol_find(lol, "b")
#' lol_count(lol, "b")
#'
#' lol <- list(list(b = 1), a = list(b = 2))
#' lol_find(lol, "b")
#' lol_find(lol, "b", not_in = "a")
#' lol_find(lol, "b", filter = "a.b")
#' str( lol_find(lol, "b", simplify = FALSE) )
#'
#' @export
lol_find <-
    function(x = list(), key = "", not_in = character(), simplify = TRUE,
             filter = NA_character_)
{
    stopifnot(
        .is_scalar_character(key),
        is.character(not_in) && !anyNA(not_in),
        .is_scalar_logical(simplify),
        .is_scalar_character(filter, na.ok = TRUE),
        `'simplify' must be TRUE when 'filter' is not NA`=
            is.na(filter) || (simplify && !anyNA(filter))
    )

    result <- .lol_visit(x, identity, key, not_in)

    if (length(result) && simplify)
        result <- unlist(result)
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
