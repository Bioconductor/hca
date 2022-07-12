.as_list_hca <- function(x)
{
    list_hca <- as.list(x)
    class(list_hca) <- c("list_hca", class(list_hca))
    list_hca
}

.hca_pagination.list_hca <-
    function(x)
{
    x$pagination
}

#' @rdname list_hca
#' @md
#'
#' @title 'list' representation of HCA query results
#'
#' @description `projects()`, `samples()`, `files()` and `bundles()`
#'     return results for the number of records indicated by the
#'     `size=` argument. Use `as = "list"` to return results as a
#'     `"list_hca"` list.
#'
#' @description `hca_next()` returns a list containing the next 'page'
#'     of results.
#'
#' @param x a 'list' returned by `projects()`, `samples()`, `files()`,
#'     or `bundles()`.
#'
#' @param size the (non-negative integer) number of elements to
#'     retrieve in the page request. The default is the number of
#'     elements requested in `x`.
#'
#' @return `hca_next()` returns a list containing the next 'page' of
#'     results.
#'
#' @examples
#' projects <- projects(size = 5, as = "list") # projects 1-5
#' next_projects <- hca_next(projects)         # projects 6-10
#'
#' @export
hca_next.list_hca <-
    function(x, size)
{
    response <- .hca_next(x, size)
    .as_list_hca(response$content)
}

#' @rdname list_hca
#'
#' @description `hca_prev()` returns a list containing the previous
#'     'page' of results.
#'
#' @return `hca_prev()` returns a list containing the previous 'page'
#'     of results.
#'
#' @examples
#' hca_prev(next_projects)            # projects 1-5
#'
#' @export
hca_prev.list_hca <-
    function(x, size)
{
    response <- .hca_prev(x, size)
    .as_list_hca(response)
}

#' @export
.hca_bind.list_hca <-
    function(x, y)
{
    result <- x
    result$hits <- c(x$hits, y$hits)
    attributes(result) <- attributes(y)
    result$pagination$prev <- x$pagination$prev
    result
}             
