test_that("'manifest()' works with default arguments", {
    test_manifest <- manifest()
    expect_true(tibble::is_tibble(test_manifest))
    expect_true(inherits(test_manifest, "manifest_tbl_hca"))
    expect_true(nrow(test_manifest) > 0L)
})

