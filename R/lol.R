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

.lol_select <- local({
    result <- setNames(list(), character())
    function(x, key, not_in, path = character(), top = TRUE) {
        if (top)
            result <<- setNames(list(), character())
        if (is.null(names(x))) {
            if (is.list(x))
                lapply(x, .lol_select, key, not_in, path, FALSE)
        } else {
            x <- x[!names(x) %in% not_in]
            nms <- names(x)
            found <- nms %in% key
            path_idx <- nzchar(path) > 0L
            nms[path_idx] <- paste(path[path_idx], nms, sep=".")
            if (is.list(x)) {
                for (i in seq_along(x))
                    if (found[[i]]) {
                        x0 <- setNames(x[i], nms[[i]])
                        result <<- append(result, x0)
                    } else {
                        .lol_select(x[[i]], key, not_in, nms[[i]], FALSE)
                    }
            } else {
                x0 <- as.list(setNames(x, nsm))
                result <<- append(result, x0)
            }
        }
        result
    }
})

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
#' @param key character() desired node name, or path to nodes of
#'     interest, e.g., `"a.b"` finds "b" nodes that are descendents
#'     (children or more distant) of "a" nodes.
#'
#' @param not_in character() node(s) whose descendants are excluded
#'     from matching `key`.
#'
#' @param filter character(1) regular expression matching paths to be
#'     returned.
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
#' 
#' ## empty key
#' lol_find(list(a = list(), a = list(1)), "a")  # c(NA, 1)
#'
#' ## 'a.b': choose 'b' nodes that are children of 'a' nodes
#' lol <- list(a = list(b = 1), c = list(b = 2))
#' lol_find(lol, "a.b")
#' lol_find(lol, "b", filter = "a.b")  # same as previous, but less robust
NULL

## helper -- convert NULL or list() elments to NA
.lol_list0_as_NA <-
    function(elt)
{
    if (is.null(elt)) {
        NA
    } else {
        elt[lengths(elt) == 0L] <- NA
        elt
    }
}

.lol_find_core <-
    function(x, keys, not_in, list0.as.NA = TRUE)
{
    stopifnot(
        .is_character(keys),
        .is_character(not_in)
    )

    if (nzchar(keys))
        keys <- strsplit(keys, ".", fixed = TRUE)[[1]]

    ## find all first keys
    x <- .lol_visit(x, identity, head(keys, 1), not_in)
    for (key in tail(keys, -1))
        ## and for each first key, find nested keys
        x <- lapply(x, .lol_visit, identity, key, not_in)

    if (list0.as.NA) { # empty list elements to NA
        x <- .lol_visit(x, .lol_list0_as_NA, head(keys, 1), character())
        for (key in tail(keys, -1))
            x <- lapply(x, .lol_visit, .lol_list0_as_NA, key, character())
    }

    .lol_select(x, head(keys, 1), not_in)
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
#' @description `lol_lfind()` is like `lol_find()`, but returns values
#'     as lists-of-lists.
#'
#' @param simplify logical(1). For `lol_lfind()`, when TRUE, unlist
#'     each value before adding, as a list element, to the list of
#'     found items. This is appropriate when the node values are
#'     vectors, or sets with homogenous scalar types. See examples.
#'
#' @return `lol_lfind()` returns an unnamed list of elements matching
#'     `key`.
#'
#' @examples
#' lol <- list(a = list(b = c(1, 2)), a = list(b = 3))
#' lol_find(lol, "b")  # names() mangled, length() of input and result differ
#' lol_lfind(lol, "b")
#'
#' lol <- list(
#'     person = list(
#'         age = 32,
#'         name = list(first = "Ima", last = "Person")
#'     ),
#'     person = list(
#'         age = 27,
#'         name = list(first = "Iman", last = "Other")
#'     )
#' )
#' lol_lfind(lol, "name")  # simplify names to vectors, e.g., c("Ima", "Person")
#' lol_lfind(lol, "name", simplify = FALSE)  # retain structure of each element
#'
#' @export
lol_lfind <-
    function(x = list(), key = "", not_in = character(), simplify = TRUE)
{
    stopifnot(.is_scalar_logical(simplify))

    x <- .lol_find_core(x, key, not_in, list0.as.NA = FALSE)

    if (simplify)
        x <- lapply(x, unlist, use.names = FALSE)

    x
}


#' @rdname lol
#'
#' @md
#'
#' @description `lol_hits()` selects an element `hits` from x, and
#'     finds `key` values in each hit. 
#'
#' @return `lol_hits()` returns either a vector (if `key` evaluates to
#'     a 0- or 1-length value for each element of `hit` or
#'     list-of-vectors with each element matching `key`. In either
#'     case, the length of the `hits` component of `x` equaals the
#'     length of the return value.
#'
#' @examples
#'
#' x <- list(
#'     hits = list(
#'         list(projects = list(projectTitle = "A title")),
#'         list(projects = list(projectTitle = "Another title")),
#'         list(files = list(projectTitle = "And another title"))
#'     ),
#'     termFacets = list(projects = list(projectTitle = c("A", "B", "C")))
#' )
#' lol_hits(x, "projects.projectTitle") # c("A title", "Another title")
#'
#' @export
lol_hits <-
    function(x = list(hits = list()), key = "", not_in = character())
{
    stopifnot(
        is.list(x),
        .is_scalar_character(key),
        .is_character(not_in)
    )

    hits <- .lol_select(x, "hits", not_in)
    stopifnot(
        `failed to find 'hits' element` =
            length(hits) == 1L && identical(names(hits), "hits")
    )
    hits <- unlist(hits, recursive = FALSE, use.names = FALSE)

    results <- lapply(hits, lol_lfind, key, not_in)
    idx <- lengths(results) > 0L
    results[idx] <- unlist(results[idx], recursive = FALSE, use.names = FALSE)
    results[!idx] <- list(NULL)
    if (length(results) && all(lengths(results) == 1L)) # simplify to vector?
        results <- unlist(results)

    results
}
