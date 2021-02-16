test_that("'projects()' works with small size and default arguments", {
    test_proj <- projects(size = 4)
    expect_true(tibble::is_tibble(test_proj))
    expect_true(nrow(test_proj) > 0L)
    expect_equal(names(test_proj),
                 projects_default_columns("tibble")$name)
})

test_that("'projects()' works for projectTitles with special characters", {
    test_proj <- projects(
        filters = filters(projectTitle = list(is = "Precursors of human CD4+ cytotoxic T lymphocytes identified by single-cell transcriptome analysis"))
    )
    expect_equal(test_proj$projectId[[1]],
                 "90bd6933-40c0-48d4-8d76-778c103bf545")
})

test_that("'projects()' works for projectTitles with line breaks", {
    expect_error(
        projects(
            filters = filters(projectTitle = list(is = "Precursors of human CD4+
            cytotoxic T lymphocytes identified by single-cell
            transcriptome analysis"))
        )
    )
})

test_that("'projects()' works for projectTitles that don't exist", {
    test_proj <- projects(
        filters = filters(projectTitle = list(is = "blah blah blah"))
    )
    expect_true(tibble::is_tibble(test_proj))
    expect_true(nrow(test_proj) == 0L)
})

test_that("'projects_default_columns()' works", {

    ## testing tibble output
    tbl <- projects_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})
