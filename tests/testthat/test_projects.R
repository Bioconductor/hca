test_that("'.index_path()' works for projects", {
    expect_equal(
        .index_path(filters = URLencode("{}", reserved = TRUE),
                    size = 10L,
                    sort = "projectTitle",
                    order = "asc",
                    catalog = "dcp2",
                    base_path = "/index/projects"),
        "/index/projects?filters=%7B%7D&size=10&sort=projectTitle&order=asc&catalog=dcp2"
    )
})

test_that("'.index_GET()' works for projects", {

    ## testing tibble output
    test_filter <- filters(organ = list(is = "pancreas"))

    test_resp <- .index_GET(filters = test_filter,
                            size = 100,
                            sort = "projectTitle",
                            order = "asc",
                            catalog = "dcp2",
                            base_path = "/index/projects")
    expect_equal(test_resp$status_code, 200)

    test_content <- test_resp$content
    expect_true(tibble::is_tibble(.projects_as_tibble(test_content)))
})
