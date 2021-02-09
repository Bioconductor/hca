test_that("testing .index_GET() with default parameters", {
    baseline_resp <- httr::GET("https://service.azul.data.humancellatlas.org/index/projects?catalog=dcp2&size=1000&sort=projectTitle&order=asc")

    test_resp <- .index_GET()
    expect_equal(test_resp$status_code, baseline_resp$status_code)
    expect_equal(test_resp$content, httr::content(baseline_resp))
})
