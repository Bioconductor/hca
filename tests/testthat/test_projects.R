test_that(".projects_parameters_*() works", {
    parameters_path <- .parameters_path(
        filters = URLencode("{}", reserved = TRUE),
        size = 10L, sort = "projectTitle", order = "asc",
        catalog = "dcp2"
    )
    expect_equal(
        parameters_path,
        "filters=%7B%7D&size=10&sort=projectTitle&order=asc&catalog=dcp2"
    )

    expect_equal(
        .index_path("/index/projects", parameters_path),
        "/index/projects?filters=%7B%7D&size=10&sort=projectTitle&order=asc&catalog=dcp2"
    )

    ## testing tibble output
    test_filter <- filters(organ = list(is = "pancreas"))
    test_parameters <- .parameters_path(filters =
                                            .filters_encoding(test_filter))
    expect_equal(
        test_parameters,
        "filters=%7B%22organ%22%3A%7B%22is%22%3A%5B%22pancreas%22%5D%7D%7D"
    )

    test_url_path <- .index_path(base_path = "/index/projects",
                                 parameters = test_parameters)
    expect_equal(
        test_url_path,
        "/index/projects?filters=%7B%22organ%22%3A%7B%22is%22%3A%5B%22pancreas%22%5D%7D%7D"
    )

    test_resp <- .hca_GET(test_url_path)
    expect_equal(test_resp$status_code, 200)

    test_content <- test_resp$content
    expect_true(tibble::is_tibble(.projects_as_tibble(test_content)))
})
