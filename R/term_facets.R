#' @importFrom tidyr unnest_wider
#'
#' @importFrom dplyr bind_cols
.term_facets_1 <-
    function(termFacets, facet)
{
    tbl <- tibble(
        term = lol_find(termFacets, paste0(facet, ".term")),
        count = lol_find(termFacets, paste0(facet, ".count"))
    )
    bind_cols(facet = facet, tbl)
}

#' @importFrom dplyr bind_rows group_by summarize arrange desc
.term_facets_n <-
    function(termFacets, facets)
{
    facets <- setdiff(facets, "organismAge")
    terms <- lapply(facets, .term_facets_1, termFacets = termFacets)
    bind_rows(terms) %>%
        group_by(.data$facet) %>%
        summarize(n_terms = length(.data$term), n_values = sum(.data$count))
}

.term_facets <-
    function(lol, facet = character())
{
    termFacets <- lol_lfind(lol, "termFacets", simplify = FALSE)[["termFacets"]]
    if (!length(facet))
        facet <- names(termFacets)

    stopifnot(
        is.character(facet), length(facet) > 0L, !anyNA(facet),
        all(facet %in% names(termFacets))
    )

    if (length(facet) == 1L) {
        .term_facets_1(termFacets, facet)
    } else {
        .term_facets_n(termFacets, facet)
    }
}
