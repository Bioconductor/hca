test_that("'files()' works with small size and default arguments", {
    test_files <- files(size = 4)
    expect_true(tibble::is_tibble(test_files))
    expect_true(nrow(test_files) > 0L)
    expect_equal(names(test_files),
                 files_default_columns("tibble")$name)
})

test_that("'files_default_columns()' works", {

    ## testing tibble output
    tbl <- files_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})

test_that("'files_download()' checks arguments correctly", {
    empty_tbl <- tibble::tibble()
    expect_error(files_download(empty_tbl),
                 'inherits\\(tbl, "files_tbl_hca"\\) is not TRUE')

    expect_error(.tbl_hca_column_check(empty_tbl,
                                         .FILES_REQUIRED_COLUMNS,
                                         "files_tbl_hca"))
})
