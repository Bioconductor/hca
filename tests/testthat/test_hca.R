test_that("'.hca_GET()' works", {
    ## valid path
    test_filter <- filters(organ = list(is = "pancreas"))
    test_parameters <- .projects_parameters_path(filters = test_filter$encoding)
    test_url_path <- .projects_index_path(parameters = test_parameters)
    resp <- .hca_GET(test_url_path)
    expect_equal(resp$status_code, 200)
    ## add test to ensure content is as expected
})

test_that("'.hca_GET' validates arguments", {
    ## not a valid subpath
    expect_error(.hca_GET("/blahblahblah"), ".*Forbidden")
    ## empty subpath
    expect_error(.hca_GET(), '.*argument "path" is missing')
})
