test_that("'bundles()' works with small size and default arguments", {
    test_bundles <- bundles(size = 4, catalog = "dcp2")
    expect_true(tibble::is_tibble(test_bundles))
    expect_true(nrow(test_bundles) > 0L)
    expect_equal(names(test_bundles),
                 bundles_default_columns()$name)
})

test_that("'bundles_default_columns()' works", {

    ## testing tibble output
    tbl <- bundles_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})
