test_that("'.hca_GET()' works", {
    ## valid path
    test_filter <- filters(organ = list(is = "pancreas"))
    test_parameters <- .parameters(filters = test_filter$encoding)
    test_url_path <- .construct_url_path(parameters = test_parameters)
    resp <- .hca_GET(test_url_path)
    expect_equal(resp$status_code, 200)

})

test_that("'.hca_GET' validates arguments", {
    ## not a valid subpath
    expect_error(.hca_GET("blahblahblah"), ".*Could not resolve host")
    ## empty subpath
    expect_error(.hca_GET(), '.*argument "path" is missing')
})
