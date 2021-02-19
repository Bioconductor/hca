test_that("'samples()' works with small size and default arguments", {
    test_proj <- samples(size = 4)
    expect_true(tibble::is_tibble(test_proj))
    expect_true(nrow(test_proj) > 0L)
    expect_equal(names(test_proj),
                 samples_default_columns("tibble")$name)
})

test_that("'samples_default_columns()' works", {

    ## testing tibble output
    tbl <- samples_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})

