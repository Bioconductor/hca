test_that("'.index_GET()' works for bundles", {

    bundles_titles <- c("Tabula Muris: Transcriptomic characterization of
                     20 organs and tissues from Mus musculus at single cell
                     resolution", "A Single-Cell Transcriptomic Map of the
                     Human and Mouse Pancreas Reveals Inter- and Intra-cell
                     Population Structure")
    bundles_filters <- filters(projectTitle = list(is = bundles_titles))

    bundles_resp <- .index_GET(filters = bundles_filters,
                               size = 100,
                               sort = "projectTitle",
                               order = "asc",
                               catalog = "dcp2",
                               base_path = "/index/bundles")
    expect_equal(bundles_resp$status_code, 200)
})

test_that("'.bundles_as_tibble()' works", {

    bundles_resp <- .index_GET(filters = filters(),
                               size = 400,
                               sort = "projectTitle",
                               order = "asc",
                               catalog = "dcp2",
                               base_path = "/index/bundles")

    tbl <- bundles_default_columns()
    expect_true(tibble::is_tibble(tbl))
    expect_true(nrow(tbl) > 0L && all(c("name", "path") %in% names(tbl)))
})
