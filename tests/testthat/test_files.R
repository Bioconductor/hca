test_that("'.index_path()' works for files", {

    files_titles <- c("Tabula Muris: Transcriptomic characterization of
                     20 organs and tissues from Mus musculus at single cell
                     resolution", "A Single-Cell Transcriptomic Map of the
                     Human and Mouse Pancreas Reveals Inter- and Intra-cell
                     Population Structure")
    files_filters <- filters(projectTitle = list(is = files_titles))

    files_index_path <- .index_path(filters = .filters_encoding(files_filters),
                                    base_path = "/index/files")

    expect_equal(
        files_index_path,
        "/index/files?filters=%7B%22projectTitle%22%3A%7B%22is%22%3A%5B%22Tabula%20Muris%3A%20Transcriptomic%20characterization%20of%5Cn%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%2020%20organs%20and%20tissues%20from%20Mus%20musculus%20at%20single%20cell%5Cn%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20resolution%22%2C%22A%20Single-Cell%20Transcriptomic%20Map%20of%20the%5Cn%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Human%20and%20Mouse%20Pancreas%20Reveals%20Inter-%20and%20Intra-cell%5Cn%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Population%20Structure%22%5D%7D%7D"
    )
})

test_that("'.index_GET()' works for files", {

    files_titles <- c("Tabula Muris: Transcriptomic characterization of
                     20 organs and tissues from Mus musculus at single cell
                     resolution", "A Single-Cell Transcriptomic Map of the
                     Human and Mouse Pancreas Reveals Inter- and Intra-cell
                     Population Structure")
    files_filters <- filters(projectTitle = list(is = files_titles))

    files_resp <- .index_GET(filters = files_filters,
                             size = 100,
                             sort = "projectTitle",
                             order = "asc",
                             catalog = "dcp2",
                             base_path = "/index/files")
    expect_equal(files_resp$status_code, 200)

    files_content <- files_resp$content
    columns <- tibble_default_columns("files", "character")
    expect_true(tibble::is_tibble(.as_tibble(files_content, columns)))
})
