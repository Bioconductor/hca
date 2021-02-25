#' @importFrom tidyr unnest_wider
#'
#' @importFrom dplyr bind_cols
.term_facets_1 <-
    function(termFacets, facet)
{
    term <- lol_lpull(termFacets, paste0(facet, ".terms[*].term"))
    idx <- vapply(term, is.null, logical(1))
    term[idx] <- list(NA_character_)
    count <- lol_lpull(termFacets, paste0(facet, ".terms[*].count"))
    idx <- vapply(count, is.null, logical(1))
    count[idx] <- list(NA_character_)
    tbl <- tibble(
        term = unlist(term, use.names = FALSE),
        count = unlist(count, use.names = FALSE)
    )
    bind_cols(facet = facet, tbl)
}

#' @importFrom dplyr %>% bind_rows group_by summarize arrange desc
.term_facets_n <-
    function(termFacets, facets)
{
    ## FIXME manage organismAge, which has term.unit and term.value paths
    facets <- setdiff(facets, "organismAge")
    terms <- lapply(facets, .term_facets_1, termFacets = termFacets)
    bind_rows(terms) %>%
        group_by(.data$facet) %>%
        summarize(n_terms = length(.data$term), n_values = sum(.data$count))
}

.term_facets <-
    function(x, facet = character())
{
    termFacets <- x$termFacets
    facet_names <- names(termFacets)
    termFacets <- lol(termFacets)
    stopifnot(
        is.character(facet), !anyNA(facet),
        all(facet %in% facet_names)
    )

    if (length(facet) == 0L) {
        .term_facets_n(termFacets, facet_names)
    } else if (length(facet) == 1L) {
        .term_facets_1(termFacets, facet)
    } else {
        .term_facets_n(termFacets, facet)
    }
}
