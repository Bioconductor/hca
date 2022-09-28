#' Catalogs Available in the HCA
#'
#' @rdname catalogs
#'
#' @name catalogs
#'
#' @description `catalogs()` queries the API for all available project catalogs
#'
#' @param catalog `character(1)` default catalog. When missing or
#'     NULL, the catalog defined by the Human Cell Atlas API is used;
#'     this is usually the most recently available catalog. Providing
#'     a non-null argument changes the default globally; restore
#'     default order by explicitly defining the arugment `catalog =
#'     NULL`.
#'
#' @return character() vector of available catalogs. The first is the
#'     default, defined by the API or by the user with argument
#'     `catalog`.
#'
#' @examples catalogs()
#'
#' @export
catalogs <- local({
    ## create a 'local' environment with variable CATALOGS
    CATALOGS <- NULL
    ## this function is the value assigned to `catalogs`
    function(catalog = NULL) {
        stopifnot(
            is.null(catalog) || .is_scalar_character(catalog)
        )
        ## first time evaluation --  CATALOGS is NULL, so assign it
        if (is.null(CATALOGS) || !missing(catalog)) {
            json <- .hca_GET("/index/catalogs")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `CATALOGS` in the local environment; the assignment
            ## persists for the session, so catalogs() is only
            ## expensive once
            default_catalog <- json$content$default_catalog
            catalogs <- names(json$content$catalogs)
            if (is.character(catalog) && !catalog %in% catalogs) {
                available_catalogs <- paste(catalogs, collapse = "' '")
                stop(
                    "user-specified catalog not available\n",
                    "    catalog: '", catalog, "'\n",
                    "    available catalogs: '", available_catalogs, "'"
                )
            }
                    
            CATALOGS <<- unique(c(catalog, default_catalog, catalogs))
        }
        CATALOGS
    }
})
