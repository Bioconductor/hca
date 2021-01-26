## helper functions
## internal only
.index_validate <-
    function(filters,
            size,
            sort,
            order,
            catalog)
{
    stopifnot(
        `use 'filters()' to create 'filter=' argument` =
            inherits(filters, "filters"),
        length(size) == 1L || size < 0 && size <= 1000
        ## sort, order, catalog already validated by match.arg
    )
}

.index_path <- function(..., base_path) {
    params <- list(...)
    parameters_path <- paste(
        names(params), unname(params), sep = "=", collapse = "&"
    )
    paste0(base_path, "?", parameters_path)
}

.index_GET <- function(filters,
                       size,
                       sort,
                       order = c("asc", "desc"),
                       catalog = c("dcp2", "it2", "dcp1", "it1"),
                       base_path)
{
    ## validate
    size <- as.integer(size)
    sort <- match.arg(sort, facet_options())
    order <- match.arg(order) # defaults from argument
    catalog <- match.arg(catalog) # defaults from argument

    ## validate
    .index_validate(
        filters = filters,
        size = size,
        sort = sort,
        order = order,
        catalog = catalog
    )

    index_path <- .index_path(
        filters = .filters_encoding(filters),
        size = size,
        sort = sort,
        order = order,
        catalog = catalog,
        base_path = base_path)

    response <- .hca_GET(index_path)
}
