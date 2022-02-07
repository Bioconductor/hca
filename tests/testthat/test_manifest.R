test_that("'manifest()' works with default arguments", {
    skip("manifest() generates an internal server 500 error, 7 February, 2022")
    test_manifest <- manifest()
    expect_true(tibble::is_tibble(test_manifest))
    expect_true(inherits(test_manifest, "manifest_tbl_hca"))
    expect_true(nrow(test_manifest) > 0L)
})

test_that("'manifest()' works with specific filter criteria", {
    test_filter <- filters(
        fileFormat = list(is = "loom"),
        workflow = list(is = c("optimus_v4.2.2", "optimus_v4.2.3"))
    )
    test_manifest <- manifest(filters = test_filter)
    expect_true(tibble::is_tibble(test_manifest))
    expect_true(inherits(test_manifest, "manifest_tbl_hca"))
    expect_true(nrow(test_manifest) > 0L)

    expect_true(all(test_manifest$file_format == 'loom'))
})
