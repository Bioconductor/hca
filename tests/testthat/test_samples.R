test_that("'samples()' works with small size and default arguments", {
    test_samples <- samples(size = 4, catalog = "dcp2")
    expect_true(tibble::is_tibble(test_samples))
    expect_true(nrow(test_samples) > 0L)
    expect_equal(names(test_samples),
                 samples_default_columns("tibble")$name)
})

test_that("'samples_default_columns()' works", {

    ## testing tibble output
    tbl <- samples_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})

