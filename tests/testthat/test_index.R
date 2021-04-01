test_that("testing .index_GET() with (almost) default parameters", {
    url <- paste0(
        "https://service.azul.data.humancellatlas.org/index/projects",
        "?catalog=dcp2&size=5&sort=projectTitle&order=asc"
    )
    baseline_resp <- httr::GET(url)

    test_resp <- .index_GET(size = 5, catalog = "dcp2")
    expect_equal(test_resp$status_code, baseline_resp$status_code)
    expect_equal(test_resp$content, httr::content(baseline_resp))
})
