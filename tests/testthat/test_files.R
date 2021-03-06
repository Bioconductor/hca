test_that("'files()' works with small size and default arguments", {
    test_proj <- files(size = 4)
    expect_true(tibble::is_tibble(test_proj))
    expect_true(nrow(test_proj) > 0L)
    expect_equal(names(test_proj),
                 files_default_columns("tibble")$name)
})

test_that("'files_default_columns()' works", {

    ## testing tibble output
    tbl <- files_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})
