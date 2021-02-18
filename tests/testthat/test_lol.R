test_that("'lol()' works", {
    x <- lol()
    expect_s3_class(x, "lol")
    expect_identical(.lol_lol(x), list())
    expect_identical(.lol_dict(x), structure(list(), class = c("dict", "list")))
    expect_identical(
        lol_path(x),
        tibble(path = character(), n = integer(), is_leaf = logical())
    )
})

test_that("'lol_select()' works", {
})

test_that("'lol_filter()' works", {
})

test_that("'lol_lpull()' works", {
})

test_that("'lol_pull()' works", {
})
