test_that("'.default_columns()' works", {
    views <- c("projects", "files", "samples", "bundles")
    for (view in views) {
        elt <- .default_columns(view, "character")
        expect_true(is.character(elt))
        expect_true(length(elt) > 0L)
        expect_true(!is.null(names(elt)))
    }
})

test_that("'.tbl_hca_name_columns()' works", {
    content <- list(hits = list(list(foo = 1, baz = list(list(bar = 2)))))
    columns0 <- c("hits[*].foo", "hits[*].baz[*].bar")

    ## abbreviate 'columns0' query by droping hits[*] and all [*] of leaf nodes
    columns <- .tbl_hca_name_columns(columns0)
    expect_identical(names(columns), c("foo", "baz.bar"))

    ## ... names propagate through .as_tbl_hca
    tbl <- .as_tbl_hca(content, columns, "files_tbl_hca")
    expect_identical(names(tbl), c("foo", "baz.bar"))

    ## ... automatic naming
    tbl0 <- .as_tbl_hca(content, columns0, "files_tbl_hca")
    expect_identical(tbl0, tbl)

    ## ... of only "" elements
    columns1 <- c(foo1 = "hits[*].foo", "hits[*].baz[*].bar")
    tbl1 <- .as_tbl_hca(content, columns1, "files_tbl_hca")
    expect_identical(names(tbl1), c("foo1", "baz.bar"))

    ## ... degenerate case
    tbl2 <- .as_tbl_hca(list(), character(), "files_tbl_hca")
    expect_identical(dim(tbl2), c(0L, 0L))
})
