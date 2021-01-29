test_that("'.index_GET()' works for samples", {

    samples_titles <- c("Tabula Muris: Transcriptomic characterization of
                     20 organs and tissues from Mus musculus at single cell
                     resolution", "A Single-Cell Transcriptomic Map of the
                     Human and Mouse Pancreas Reveals Inter- and Intra-cell
                     Population Structure")
    samples_filters <- filters(projectTitle = list(is = samples_titles))

    samples_resp <- .index_GET(filters = samples_filters,
                             size = 100,
                             sort = "projectTitle",
                             order = "asc",
                             catalog = "dcp2",
                             base_path = "/index/samples")
    expect_equal(samples_resp$status_code, 200)
})

test_that("'.samples_as_tibble()' works", {

    samples_resp <- .index_GET(filters = filters(),
                                    size = 400,
                                    sort = "projectTitle",
                                    order = "asc",
                                    catalog = "dcp2",
                                    base_path = "/index/samples")

    samples_content <- samples_resp$content
    columns <- tibble_default_columns("samples", "character")
    expect_true(tibble::is_tibble(.as_tibble(samples_content, columns)))
})
