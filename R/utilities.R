## common validity checks

#' @export
.is_vector_0 <- function(x)
    length(x) == 0L

#' @export
.is_character_0 <- function(x)
    is.character(x) && .is_vector_0(x)

#' @export
.is_scalar <- function(x, na.ok = FALSE) {
    length(x) == 1L && (na.ok || !anyNA(x))
}

#' @export
.is_scalar_logical <- function(x, na.ok = FALSE) {
    is.logical(x) && .is_scalar(x, na.ok)
}

#' @export
.is_scalar_integer <- function(x, na.ok = FALSE) {
    is.integer(x) && .is_scalar(x, na.ok)
}

#' @export
.is_scalar_numeric <- function(x, na.ok = FALSE) {
    is.numeric(x) && .is_scalar(x, na.ok)
}

#' @export
.is_scalar_character <- function(x, na.ok = FALSE) {
    is.character(x) && .is_scalar(x, na.ok)
}

#' @export
.is_character <- function(x, na.ok = FALSE) {
    is.character(x) && (na.ok || !anyNA(x))
}

#' @export
.is_catalog <- function(x, na.ok = FALSE) {
    .is_scalar_character(x) && (x %in% catalogs())
}

## wrap lines to output width, with 2-character 'exdent' of lines
## after the first

.wrap_lines <-
    function(x, exdent = 2)
{
    ## create a length-1 string
    x <- paste(x, collapse = " ")

    ## break into lines of fixed width
    lines <- strwrap(x, exdent = exdent)

    ## collapse into a single string with embedded new lines
    paste(lines, collapse = "\n")
}        
