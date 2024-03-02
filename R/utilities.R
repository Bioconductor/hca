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
.is_catalog <- function(x) {
    .is_scalar_character(x) && (x %in% catalogs())
}

.is_filter <- function(x) {
    all(inherits(x, c("filters", "hca"), which = TRUE) == 1:2)
}

.is_project_id <- local({
    HEX_PATTERN <- paste0("^", paste0(
        "[0-9abcdef]",
        "{", c(8L, 4L, 4L, 4L, 12L), "}",
        collapse = "-"
    ), "$")
    function(x)
        grepl(HEX_PATTERN, x)
})

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

## .onLoad

.onLoad <-
    function(libname, pkgname)
{
    .BASE_URL <<- Sys.getenv("BIOCONDUCTOR_HCA_BASE_URL", .BASE_URL)

    ## unlink cache on build system once every two weeks
    if (identical(Sys.getenv("IS_BIOC_BUILD_MACHINE"), "true")) {
        cache_path <- manifest_cache()
        if (dir.exists(cache_path)) {
            creation_time <-  file.info(cache_path, extra_cols = FALSE)$ctime
            age <- difftime(Sys.Date(), creation_time, units = "days")
            if (age > 14) {
                unlink(cache_path, recursive = TRUE, force =  TRUE)
            }
        }
    }
}
