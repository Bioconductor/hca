.lol_visit_impl <- function(x, path, index, dict)
    UseMethod(".lol_visit_impl")

## default implementation of .lol_visit_impl
.lol_visit_impl.default <- function(x, path, index, dict) {
    dict[[path]] <- append(dict[[path]], list(index))
    attr(dict[[path]], "leaf") <- TRUE
}

## list implementation of .lol_visit_impl

## this function is applied recursively to traverse a nested list,
## which can be though of as a tree-like structure,
## the first iteration occurring at the root node
.lol_visit_impl.list <- function(x, path, index, dict) {
    ## dict maintains the results of the traversal across iterations
    ## building out the various paths
    dict[[path]] <- append(dict[[path]], list(index))
    attr(dict[[path]], "leaf") <- FALSE

    nms <- names(x)
    is_null_nms <- is.null(nms)
    ## if the list x is unnamed, [*] is used,
    ## indicating a wild card matching any integer
    if (is_null_nms) {
        ## nms <- paste0("[[", seq_along(x), "]]")
        nms <- rep("[*]", length(x))
    }
    ## logic for building out path names
    ## starting with current node of the nested list and extending the path by
    ## appending the next set of names or wild cards
    if (identical(path, ".")) {
        path <- nms
    } else {
        if (is_null_nms) {
            path <- paste0(path, nms)
        } else {
            path <- paste0(path, ".", nms)
        }
    }

    ## recursively apply function until leaf is hit
    for (i in seq_along(x))
        .lol_visit_impl(x[[i]], path[[i]], append(index, i), dict)
}

.lol <-
    function(lol, dict, path = .lol_path(dict), class = "lol")
{
    if (!inherits(dict, "dict"))
        class(dict) <- c("dict", class(dict))
    structure(
        list(lol = lol, dict = dict, path = path),
        class = class
    )
}

#' @rdname lol
#' @md
#'
#' @title Representing and manipulating list-of-list data structures.
#'
#' @description `lol()` constructs an indexed representation of an R
#'     'list-of-lists', typically from JSON queries. The object is
#'     conveniently manipulated by other functions on this page to
#'     filter and select subsets of the structure, and to pull
#'     individual paths from across the list-of-lists.
#'
#' @param x for `lol()` an R `list`, containing, recursively, named or
#'     unnamed R lists or atomic vectors.
#'
#' @return `lol()` returns a representation of the list-of-lists. The
#'     list has been processed to a dictionary with entries to all
#'     paths through the list, as well as a tibble summarizing the
#'     path, number of occurrences, and leaf status of each unique
#'     path.
#'
#' @examples
#' plol <- projects(as = "lol")
#' plol
#'
#' @export
lol <- function(x = list()) {
    stopifnot(
        inherits(x, "list")
    )

    dict <- new.env(parent = emptyenv())
    .lol_visit_impl(x, ".", integer(), dict)
    dict <- as.list(dict)

    .lol(x, dict)
}

.lol_dict <- function(x) x[["dict"]]

.lol_lol <- function(x) x[["lol"]]

.lol_lengths <- function(x)
    UseMethod(".lol_lengths")

.lol_lengths.dict <-
    function(x)
{
    lengths(x)
}

.lol_lengths.lol <-
    function(x)
{
    .lol_lengths(.lol_dict(x))
}

.lol_is_leaf <- function(x)
    UseMethod(".lol_is_leaf")

.lol_is_leaf.dict <-
    function(x)
{
    vapply(x, attr, logical(1), "leaf")
}

.lol_is_leaf.lol <-
    function(x)
{
    .lol_is_leaf(.lol_dict(x))
}

.lol_path <-
    function(x)
{
    path <- ls(x, all.names = FALSE)
    is_leaf <- .lol_is_leaf(x)[path]
    tbl <- tibble(
        path = path,
        n = unname(.lol_lengths(x)[path]),
        is_leaf = unname(is_leaf)
    )
    arrange(tbl, .data$path)
}

.lol_valid_path <-
    function(x, path)
{
    ok <- .is_character_0(path) || path %in% lol_path(x)$path
    ok || stop("'path' not in 'x':\n", "  path: '", path, "'")
}

#' @rdname lol
#'
#' @param x an object of class 'lol'
#'
#' @param path character(1) from the tibble returned by `lol_path(x)`.
#'
#' @return `lol_select()` returns an object of class `"lol"` subset
#'     to contain just the elements matching `path` as 'top-level'
#'     elements of the list-of-lists.
#'
#' @examples
#' plol |> lol_select("hits[*].projects[*]")
#'
#' @export
lol_select <-
    function(x, path = character())
{
    stopifnot(
        inherits(x, "lol"),
        .is_character_0(path) || .is_scalar_character(path),
        .lol_valid_path(x, path)
    )

    paths <- lol_path(x)
    idx <- paths$path[startsWith(paths$path, path)]
    paths <- paths[paths$path %in% idx,]
    dict <-  .lol_dict(x)[paths$path]

    .lol(.lol_lol(x), dict, paths, class(x))
}

#' @rdname lol
#' @md
#'
#' @description `lol_filter()` filters available paths based on
#'     selections in `...`, e.g., `n` (number of matching elements) or
#'     `is_leaf` (is the element a 'leaf' in the list-of-lists
#'     representation?).
#'
#' @param ... for `lol_filter()`, named filter expressions
#'     evaluating to a logical vector with length equal to the number
#'     of rows in `lol_path()`.
#'
#' @return `lol_filter()` returns an object of class `lol`, filtered
#'     to contain elements consistent with the filter criteria.
#'
#' @examples
#' plol |>
#'    lol_select("hits[*].projects[*]") |>
#'    lol_filter(n == 44, is_leaf)
#'
#' @export
lol_filter <-
    function(x, ...)
{
    stopifnot(
        inherits(x, "lol")
    )

    path <- lol_path(x)
    ## FIXME: don't allow filtering on 'path$path'
    path <- filter(path, ...)
    dict <- .lol_dict(x)[path$path]

    .lol(.lol_lol(x), dict, path, class(x))
}

#' @rdname lol
#'
#' @description `lol_lpull()` returns a list containing elements
#'     corresponding to a single `path`.
#'
#' @return `lol_lpull()` returns a list, where each element
#'     corresponds to an element found at `path` in the list-of-lists
#'     structure `x`.
#'
#' @export
lol_lpull <-
    function(x, path)
{
    stopifnot(
        inherits(x, "lol"),
        .is_scalar_character(path),
        .lol_valid_path(x, path)
    )
    lol <- .lol_lol(x)
    value <- lapply(.lol_dict(x)[[path]], function(idx) lol[[idx]])
    names(value) <-  rep(path, length(value))
    value
}

#' @rdname lol
#'
#' @md
#'
#' @description `lol_pull()` tries to simplify the list-of-lists
#'     structure returned by `lol_lpull()` to a vector.
#'
#' @return `lol_pull()` returns an unnamed vector of elements matching
#'     `key`.
#'
#' @examples
#' plol |>
#'     lol_pull("hits[*].entryId") |>
#'     head()
#'
#' @export
lol_pull <-
    function(x, path)
{
    value <- lol_lpull(x, path)
    unlist(value, recursive = FALSE, use.names = FALSE)
}

#' @rdname lol
#'
#' @description `lol_path()` returns a tibble representing the paths
#'     through the list-of-lists, without the underlying list-of-list
#'     data.
#' @examples
#' plol |> lol_path()
#'
#' @export
lol_path <- function(x) x[["path"]]

#' @rdname lol
#'
#' @description `as.list()` returns a list-of-lists representation of
#'     the data returned by `projects()`, etc.
#'
#' @export
as.list.lol <- function(x, ...) .lol_lol(x)

#' @rdname lol
#'
#' @export
print.lol <-
    function(x, ...)
{
    lengths <- .lol_lengths(x)
    is_leaf <- .lol_is_leaf(x)
    path <- lol_path(x)
    cat(
        "# class: ", paste(class(x), collapse = " "), "\n",
        "# number of distinct paths: ", NROW(path), "\n",
        "# total number of elements: ", sum(lengths), "\n",
        "# number of leaf paths: ", sum(is_leaf), "\n",
        "# number of leaf elements: ", sum(lengths[is_leaf]), "\n",
        "# lol_path():\n",
        sep = ""
    )
    print(path, ...)
}

## class definition

.as_lol_hca <-
    function(x, keys)
{
    pagination <- x$pagination
    x <- lol(list(hits = x$hits)) #only 'remember' hits
    attr(x, "keys") <- keys
    attr(x, "pagination") <- pagination
    class(x) <- c("lol_hca", class(x))

    x
}

## pagination

.lol_hca_keys <- function(x)
    attr(x, "keys")

#' @rdname lol
#' @md
#'
#' @description `hca_next()` returns the next 'page' of results, if
#'     available.
#'
#' @param x a 'list-of-lists' returned by `projects()`, `samples()`,
#'     `files()`, or `bundles()`
#'
#' @param size the (non-negative integer) number of elements to
#'     retrieve in the page request. The default is the number of
#'     elements requested in `x`.
#'
#' @return `hca_next()` returns a list-of-lists containing the next
#'     'page' of results.
#'
#' @examples
#' projects <- projects(size = 5, as = "lol")     # projects 1-5
#' next_projects <- hca_next(projects)            # projects 6-10
#'
#' @export
hca_next.lol_hca <-
    function(x, size)
{
    response <- .hca_next(x, size)
    keys <- .lol_hca_keys(x)
    .as_lol_hca(response$content, keys)
}

#' @rdname lol
#'
#' @description `hca_prev()` returns the previous 'page' of results.
#'
#' @return `hca_prev()` returns a tibble with the same columns as `x`,
#'     containing the previous 'page' of results.
#'
#' @examples
#' hca_prev(next_projects)                        # projects 1-5
#'
#' @export
hca_prev.lol_hca <-
    function(x, size)
{
    response <- .hca_prev(x, size)
    keys <- .lol_hca_keys(x)
    .as_lol_hca(response$content, keys)
}

#' @export
.hca_bind.lol_hca <-
    function(x, y)
{
    ## bind list of hits & create a new object
    hits_x <- .lol_lol(x)[["hits"]]
    hits_y <- .lol_lol(y)[["hits"]]
    lol <- lol(list(hits = c(hits_x, hits_y)))

    class(lol) <- class(y)
    attr(lol, "keys") <- attr(y, "keys")

    ## update pagination
    pagination <- attr(y, "pagination")
    pagination[["previous"]] <- .hca_pagination(x)[["previous"]]
    attr(lol, "pagination") <- pagination

    lol
}
