test_that(".projects_parameters_*() works", {
    parameters_path <- .projects_parameters_path(
        filters = URLencode("{}", reserved = TRUE),
        size = 10L, sort = "projectTitle", order = "asc",
        catalog = "dcp2"
    )
    expect_equal(
        parameters_path,
        "filters=%7B%7D&size=10&sort=projectTitle&order=asc&catalog=dcp2"
    )

    expect_equal(
        .projects_index_path(parameters_path),
        "/index/projects?filters=%7B%7D&size=10&sort=projectTitle&order=asc&catalog=dcp2"
    )
})
