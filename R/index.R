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

.index_GET <- function(filters = NULL,
                       size = 1000L,
                       sort = "projectTitle",
                       order = c("asc", "desc"),
                       catalog = NULL,
                       base_path = "/index/projects")
{
    ## validate
    size <- as.integer(size)
    sort <- match.arg(sort, facet_options())
    order <- match.arg(order) # defaults from argument

    if(is.null(filters)){
        filters <- filters()
    }

    if(is.null(catalog)){
        catalogs <- catalogs()
        catalog <- catalogs[1]
    }

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
