## common validity checks

.is_vector_0 <- function(x)
    length(x) == 0L

.is_character_0 <- function(x)
    is.character(x) && .is_vector_0(x)

.is_scalar <- function(x, na.ok = FALSE) {
    length(x) == 1L && (na.ok || !anyNA(x))
}

.is_scalar_logical <- function(x, na.ok = FALSE) {
    is.logical(x) && .is_scalar(x, na.ok)
}

.is_scalar_integer <- function(x, na.ok = FALSE) {
    is.integer(x) && .is_scalar(x, na.ok)
}
             
.is_scalar_numeric <- function(x, na.ok = FALSE) {
    is.numeric(x) && .is_scalar(x, na.ok)
}

.is_scalar_character <- function(x, na.ok = FALSE) {
    is.character(x) && .is_scalar(x, na.ok)
}

.is_character <- function(x, na.ok = FALSE) {
    is.character(x) && (na.ok || !anyNA(x))
}
