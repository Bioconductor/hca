test_that("all_colums returns character vectors", {
    expect_true(inherits(all_columns("projects"), "character"))
    expect_true(inherits(all_columns("files"), "character"))
    expect_true(inherits(all_columns("samples"), "character"))
    expect_true(inherits(all_columns("bundles"), "character"))
})

test_that("all_colums properly names defalut columns", {
    expect_true(all(.PROJECTS_DEFAULT_COLUMNS %in% all_columns("projects")))
    expect_true(all(.FILES_DEFAULT_COLUMNS %in% all_columns("files")))
    expect_true(all(.SAMPLES_DEFAULT_COLUMNS %in% all_columns("samples")))
    expect_true(all(.BUNDLES_DEFAULT_COLUMNS %in% all_columns("bundles")))
})
