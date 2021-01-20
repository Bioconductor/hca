test_that("'lol_find()' works", {
    ## unnamed lists
    expect_identical(lol_find(), list())
    expect_identical(lol_find(list(list(), list())), list())

    ## named lists
    named_list <- setNames(list(), character())
    object <- lol_find(named_list)
    expect_identical(object, named_list)

    expect_identical(lol_find(list(list(b = 1))), list())
    expect_identical(lol_find(list(list(b = 1))), list())
    expect_identical(lol_find(list(a = list(b = 1))), named_list)

    x <- list(list(a = 1), list(a = 2), list(b = 3))
    expect_identical(lol_find(x, "a"), c(a = 1, a = 2))
    expect_identical(lol_find(x, "b"), c(b = 3))
    expect_identical(lol_find(x, "c"), list())

    x <- list(a = list(list(b = 1)), a = list(list(b = 2)))
    expect_identical(lol_find(x, "a"), c(a.b = 1, a.b = 2))
    expect_identical(lol_find(x, "b"), c(a.b = 1, a.b = 2))

    x <- list(a = list(list(b = 1)), c = list(list(b = 2)))
    expect_identical(lol_find(x, "b"), c(a.b = 1, c.b = 2))
})

test_that("'lol_find()' works for less-usual cases", {
    ## non-standard names
    x <- list(`foo/bar` = list(b = 2))
    expect_identical(lol_find(x, "b"), c(`foo/bar.b` = 2))

    ## repeated names of an element; different levels
    x <- list(a = list(b = 1, b = 2), a = list(b = 3))
    expect_identical(lol_find(x, "b"), c(a.b = 1, a.b = 2, a.b = 3))

    ## different levels of nesting
    x <- list(a = list(b = 2), a = list(c = list(b = 3)))
    expect_identical(lol_find(x, "b"), c(a.b = 2, a.c.b = 3))
    expect_identical(lol_find(x, "a"), c(a.b = 2, a.c.b = 3))

    ## 'b' in both leaf and nested position within a list
    x <- list(b = 1, a = list(b = 2))
    expect_identical(lol_find(x, "b"), c(b = 1, a.b = 2))

    x <- list(b = 1, a = list(b = 2), list(b = 3))
    expect_identical(lol_find(x, "b"), c(b = 1, a.b = 2, b = 3))

    ## named / unnamed lists
    x <- list(a = list(list(b = 1)), a = list(list(b = 2)))
    expect_identical(lol_find(x, "b"), c(a.b = 1, a.b = 2))
})

test_that("'lol_find(not_in=)' works", {
    x <- list(a = list(b = 1), c = list(b = 2), a = list(b = 3))
    expect_identical(lol_find(x, "b", "a"), c(c.b = 2))
    expect_identical(lol_find(x, "b", "c"), c(a.b = 1, a.b = 3))
    named_list <- setNames(list(), character())
    expect_identical(lol_find(x, "b", c("a", "c")), named_list)
})

test_that("'lol_find(filter=)' works", {
    x <- list(a = list(b = 1), c = list(b = 2), a = list(b = 3))
    expect_identical(lol_find(x, "b", filter = "c.b"), c(c.b = 2))
    expect_identical(lol_find(x, "b", filter = "^a"), c(a.b = 1, a.b = 3))
})
