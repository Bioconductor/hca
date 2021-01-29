test_that("'lol_find()' works", {
    named_character <- setNames(character(), character())

    ## unnamed lists
    expect_identical(lol_find(), named_character)
    expect_identical(lol_find(list(list(), list())), named_character)

    ## named lists
    expect_identical(lol_find(setNames(list(), character())), named_character)

    expect_identical(lol_find(list(list(b = 1))), named_character)
    expect_identical(lol_find(list(list(b = 1))), named_character)
    expect_identical(lol_find(list(a = list(b = 1))), named_character)

    x <- list(list(a = 1), list(a = 2), list(b = 3))
    expect_identical(lol_find(x, "a"), c(a = 1, a = 2))
    expect_identical(lol_find(x, "b"), c(b = 3))
    expect_identical(lol_find(x, "c"), named_character)

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

    ## empty elements
    x <- list(a = list(), a = list(1))
    expect_identical(lol_find(x, "a"), c(a = NA, a = 1))

    x <- list(a = list(b = 1), a = list(b = list()))
    expect_identical(lol_find(x, "b"), c(a.b = 1, a.b = NA))
})

test_that("'lol_find(not_in=)' works", {
    x <- list(a = list(b = 1), c = list(b = 2), a = list(b = 3))
    expect_identical(lol_find(x, "b", "a"), c(c.b = 2))
    expect_identical(lol_find(x, "b", "c"), c(a.b = 1, a.b = 3))
    named_character <- setNames(character(), character())
    expect_identical(lol_find(x, "b", c("a", "c")), named_character)
})

test_that("'lol_find(filter=)' works", {
    x <- list(a = list(b = 1), c = list(b = 2), a = list(b = 3))
    expect_identical(lol_find(x, "b", filter = "c.b"), c(c.b = 2))
    expect_identical(lol_find(x, "b", filter = "^a"), c(a.b = 1, a.b = 3))
})

test_that("'lol_find()' works with nested elements", {
    x <- list(a = list(b = 1), a = list())
    expect_identical(lol_find(x, "a.b"), c(a.b = 1, a = NA))

    x <- list(a = list(b = 1), a = list(), a = list(c = 1))
    expect_identical(lol_find(x, "a.b"), c(a.b = 1, a = NA, a = NA))

    x <- list(a = list(c = list(b = 1)), a = list(), a = list(b = 2))
    expect_identical(lol_find(x, "a.b"), c(a.c.b = 1, a = NA, a.b = 2))
    expect_identical(lol_find(x, "a.c"), c(a.c.b = 1, a = NA, a = NA))

    x <- list(
        a = list(c = list(b = 1)),
        a = list(c = list(d = 1)),
        a = list(c = list(b = 2)),
        a = list(b = 3)
    )
    expect_identical(
        lol_find(x, "a.b"),
        c(a.c.b = 1, a = NA, a.c.b = 2, a.b = 3)
    )
})

test_that("'lol_lfind()' works", {
    named_list <- setNames(list(), character())
    expect_identical(lol_lfind(list()), named_list)

    x <- list(a = list())
    expect_identical(lol_lfind(x), named_list)
    expect_identical(lol_lfind(x, "a"), list(a = NULL))

    x <- list(a = list(b = 1))
    expect_identical(lol_lfind(x, "a"), list(a = 1))

    x <- list(a = list(b = 1), a = list(b = 2))
    expect_identical(lol_lfind(x, "a"), list(a = 1, a = 2))

    x <- list(a = list(b = 1), a = list(), a = list(b = 2))
    expect_identical(lol_lfind(x, "a"), list(a = 1, a = NULL, a = 2))

    x <- list(a = list(b = 1),  c = list(b = 2), a = list(b = 3))
    expect_identical(lol_lfind(x, "a"), list(a = 1, a = 3))

    x <- list(a = list(b = 1, b = 2), a = list(b = 3), a = list())
    expect_identical(lol_lfind(x, "a"), list(a = c(1, 2), a = 3, a = NULL))
})

test_that("'lol_lfind(simplify = FALSE)' works", {
    named_list <- setNames(list(), character())
    expect_identical(lol_lfind(list(), simplify = FALSE), named_list)

    x <- list(a = list())
    expect_identical(lol_lfind(x, simplify = FALSE), named_list)
    expect_identical(lol_lfind(x, "a", simplify = FALSE), x)

    x <- list(a = list(b = 1))
    expect_identical(lol_lfind(x, "a", simplify = FALSE), x)

    x <- list(a = list(b = 1), a = list(b = 2))
    expect_identical(lol_lfind(x, "a", simplify = FALSE), x)

    x <- list(a = list(b = 1), a = list(), a = list(b = 2))
    expect_identical(lol_lfind(x, "a", simplify = FALSE), x)

    x <- list(a = list(b = 1),  c = list(b = 2), a = list(b = 3))
    expect_identical(lol_lfind(x, "a", simplify = FALSE), x[c(1, 3)])
})

test_that("'lol_lfind()' works with nested elements", {
    named_list <- setNames(list(), character())
    x <- list(a = list())
    expect_identical(lol_lfind(x, "a.b"), list(a = NULL))

    x <- list(a = list(b = 1))
    expect_identical(lol_lfind(x, "a.b"), list(a = 1))

    x <- list(a = list(b = 1), a = list(b = 2), a = list(c = 3))
    expect_identical(lol_lfind(x, "a.b"), list(a = 1, a = 2, a = NULL))

    x <- list(
        a = list(b = list(c = 1)),
        a = list(c = 2, c = 3),
        c = list(),
        a = list()
    )
    expect_identical(lol_lfind(x, "a.c"), list(a = 1, a = c(2, 3),  a = NULL))

    x <- list(
        a = list(b = list(c = 1)),
        a = list(b = list(c = 2), b = list(c = 3)),
        c = list(),
        a = list()
    )
    expect_identical(lol_lfind(x, "a.c"), list(a = 1, a = c(2, 3),  a = NULL))
})

test_that("'lol_hits()' works", {

    x <- list(list(hits = list()))
    expect_identical(lol_hits(x, "a"), list())

    x <- list(
        hits = list(
            list(projects = list(projectTitle = "A title")),
            list(projects = list(projectTitle = "Another title"))
        )
    )
    expect_identical(lol_hits(x, "projectTitle"), c("A title", "Another title"))

    ## preserve geometry of hits
    x <- list(
        hits = list(
            list(a = list(b = c(1, 2))),
            list(a = list()),
            list(a = list(b = 3))
        )
    )
    expect_identical(lol_hits(x, "a.b"), list(c(1, 2), NULL, 3))
    expect_identical(lol_hits(x, "b"), list(c(1, 2), NULL, 3))

})

test_that("'.lol_path_abbreviation()' works", {
    expect_identical(.lol_path_abbreviation(character()), character())
    expect_identical(.lol_path_abbreviation(letters), letters)

    expect_identical(.lol_path_abbreviation(c("x.a", "y.a")), c("x.a", "y.a"))
    expect_identical(.lol_path_abbreviation(c("x.a.b", "y.b")), c("a.b", "y.b"))

    expect_identical(.lol_path_abbreviation(c("a.b", "b.c")), c("a.b", "c"))
})
