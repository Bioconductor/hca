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
                 'inherits\\(tbl, "tbl_hca"\\) is not TRUE')
    class(empty_tbl) <- c("tbl_hca", class(empty_tbl))
    expect_error(
        files_download(empty_tbl),
        '\'tbl=\' must contain columns "fileId", "url", and "name"'
    )
})