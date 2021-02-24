test_that("'lol()' works", {
    x <- lol()
    expect_s3_class(x, "lol")
    expect_identical(.lol_lol(x), list())
    expect_identical(.lol_dict(x), structure(list(), class = c("dict", "list")))
    expect_identical(
        lol_path(x),
        tibble(path = character(), n = integer(), is_leaf = logical())
    )

    l <- list(a = 1, b = list(c = 2))
    x <- lol(list(a = 1, b = list(c = 2)))
    expect_s3_class(x, "lol")
    expect_identical(.lol_lol(x), l)
    tbl <- tibble(
        path = c("a", "b", "b.c"),
        n = 1L,
        is_leaf = c(TRUE, FALSE, TRUE)
    )
    expect_identical(lol_path(x), tbl)
})

test_that("'lol_select()' works", {
    expect_error(lol_select(), 'argument "x" is missing, with no default')
    expect_error(lol_select(list()), 'inherits\\(x, "lol"\\) is not TRUE')

    x <- lol()
    expect_identical(lol_select(x), x)
    expect_identical(lol_select(x, character()), x)
    expect_error(lol_select(x, "foo"), "'path' not in 'x':")

    x <- lol(list(a = 1, b = list(c = 2)))
    expect_identical(lol_path(lol_select(x)), lol_path(lol()))
    expect_identical(lol_path(lol_select(x, character())), lol_path(lol()))
    expect_error(lol_select(x, "foo"), "'path' not in 'x':")

    tbl <- tibble(path = "a", n = 1L, is_leaf = TRUE)
    expect_identical(lol_path(lol_select(x, "a")), tbl)

    tbl <- tibble(path = c("b", "b.c"), n = 1L, is_leaf = c(FALSE, TRUE))
    expect_identical(lol_path(lol_select(x, "b")), tbl)

    tbl <- tibble(path = "b.c", n = 1L, is_leaf = TRUE)
    expect_identical(lol_path(lol_select(x, "b.c")), tbl)
})

test_that("'lol_filter()' works", {
    expect_error(lol_filter(), 'argument "x" is missing, with no default')
    expect_error(lol_filter(list()), 'inherits\\(x, "lol"\\) is not TRUE')

    x <- lol()
    expect_identical(lol_filter(x), x)
    expect_identical(lol_filter(x, logical()), x)
    expect_identical(lol_filter(x, logical(1)), x)
    expect_error(lol_filter(x, logical(2)))

    l <- list(a = 1, b = list(c = 2))
    x <- lol(l)
    expect_identical(lol_filter(x), x)
    expect_error(lol_filter(x, logical(0)))
    expect_identical(lol_filter(x, n == 1L), x)

    object <- lol_filter(x, .data$is_leaf)
    expect_identical(.lol_lol(object), l)
    expect_identical(lol_path(object), filter(lol_path(x), is_leaf))
    expect_identical(
        .lol_dict(object),
        structure(.lol_dict(x)[c("a", "b.c")], class = c("dict", "list"))
    )
})

test_that("'lol_lpull()' works", {
    expect_error(lol_lpull(), 'argument "x" is missing, with no default')
    expect_error(lol_lpull(list()), 'inherits\\(x, "lol"\\) is not TRUE')
})

test_that("'lol_pull()' works", {
    expect_error(lol_pull(), 'argument "x" is missing, with no default')
    expect_error(lol_pull(list()), 'inherits\\(x, "lol"\\) is not TRUE')
})
