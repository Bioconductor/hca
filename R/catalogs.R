#' Title
#' @rdname catalogs
#'
#' @name catalogs
#'
#' @description `catalogs()` queries the API for all available project catalogs
#'
#' @return character() vector of available catalogs
#' @export
#'
#' @examples catalogs()
catalogs <- function(){
    json <- .hca_GET("/index/catalogs")
    catalogs <- names(json[["content"]][["catalogs"]])
    catalogs
}