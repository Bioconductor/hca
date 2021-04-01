#' Catalogs Available in the HCA
#'
#' @rdname catalogs
#'
#' @name catalogs
#'
#' @description `catalogs()` queries the API for all available project catalogs
#'
#' @return character() vector of available catalogs
#'
#' @examples catalogs()
#'
#' @export
catalogs <- local({
    ## create a 'local' environment with variable CATALOGS
    CATALOGS <- NULL
    ## this function is the value assigned to `catalogs`
    function() {
        ## first time evaluation --  CATALOGS is NULL, so assign it
        if (is.null(CATALOGS)) {
            json <- .hca_GET("/index/catalogs")
            ## `<<-` assigns _outside_ the function, to the variable
            ## `CATALOGS` in the lcoal environment; the assignment
            ## persists for the session, so catalogs() is only
            ## expensive once
            CATALOGS <<- names(json[["content"]][["catalogs"]])
        }
        CATALOGS
    }
})
