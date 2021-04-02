.SUMMARY_PATH <- "/index/summary"

.summary_as_integer <- function(x) {
    if (max(x) > .Machine$integer.max) {
        x
    } else {
        as.integer(x)
    }
}

#' @importFrom tibble enframe
#'
#' @importFrom tidyr unnest_longer
#'
#' @importFrom dplyr %>% .data filter
.summary_overview <- function(content) {
    enframe(content) %>%
        filter(grepl("(Size|Count)$", .data$name)) %>%
        unnest_longer("value") %>%
        mutate(value = .summary_as_integer(.data$value))
}

.summary_fileTypeSummaries <- function(content) {
    records <- content$fileTypeSummaries
    tibble(
        fileType = vapply(records, `[[`, character(1), "fileType"),
        count = vapply(records, `[[`, integer(1), "count"),
        totalSize = vapply(records, `[[`, numeric(1), "totalSize")
    )
}

.summary_organTypes <- function(content) {
    records <- content$organTypes
    tibble(
        organTypes = vapply(records, `[[`, character(1), 1L)
    )
}

.summary_cellCountSummaries <- function(content) {
    records <- content$cellCountSummaries
    tibble(
        organType = lapply(records, `[[`, "organType"),
        countOfDocsWithOrganType =
            vapply(records, `[[`, integer(1), "countOfDocsWithOrganType"),
        totalCellCountByOrgan =
            vapply(records, `[[`, numeric(1), "totalCellCountByOrgan")
    ) %>%
        unnest_longer("organType")
}

#' @rdname summary
#'
#' @name summary
#'
#' @md
#'
#' @title Repository summary statistics
#'
#' @description `summary()` provides numerical summaries of catalog content
#'
#' @param filters filter object created by `filters()`, or `NULL`
#'     (default; all projects).
#'
#' @param type character(1) type of summary to return. Possible values
#'     include "overview", "fileTypeSummaries", "cellCountSummaries",
#'     "organType", and a "list" off all summary statistics.
#'
#' @param catalog character(1) source of data. Use
#'     `catalogs()` for possible values.
#'
#' @return `summary()` returns a tibble or (for `type = "list"`) a
#'     list-of-lists of summary statistics.
#'
#' @examples
#' summary()
#'
#' filter <- filters(
#'     organ = list(is = c("brain", "heart")),
#'     genusSpecies = list(is = "Homo sapiens")
#' )
#' summary(filter)
#' summary(filter, "fileTypeSummaries")
#' summary(filter, "cellCountSummaries")
#'
#' @export
summary <- function(filters = NULL,
             type = c(
                 "overview",
                 "fileTypeSummaries", "cellCountSummaries", "organTypes",
                 "list"
             ),
             catalog = NULL) {

    if (is.null(filters)){
        filters <- filters()
    }

    if(is.null(catalog)){
        catalog <- catalogs()[1]
    }

    type <- match.arg(type)
    stopifnot(
        `use 'filters()' to create 'filter=' argument` =
            inherits(filters, "filters"),
        ## catalog validation
        `catalog must be a character scalar returned by catalogs()` =
            .is_catalog(catalog)
    )
    encoding <- .filters_encoding(filters)

    parameters <- paste(
        c("filters", "catalog"),
        c(encoding, catalog),
        sep = "=", collapse = "&"
    )
    path <- paste0(.SUMMARY_PATH, "?", parameters)

    content <- .hca_GET(path)$content
    switch(
        type,
        overview = .summary_overview(content),
        fileTypeSummaries = .summary_fileTypeSummaries(content),
        organTypes = .summary_organTypes(content),
        cellCountSummaries = .summary_cellCountSummaries(content),
        list = content
    )
}
