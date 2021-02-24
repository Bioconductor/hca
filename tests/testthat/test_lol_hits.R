test_that("'lol_hits_lpull()' works", {
    expect_error(lol_hits_lpull(), 'argument "x" is missing, with no default')
    expect_error(lol_hits_lpull(list()), 'inherits\\(x, "lol"\\) is not TRUE')
    expect_error(lol_hits_lpull(lol()), 'argument "path" is missing, with no default')
    expect_error(lol_hits_lpull(lol(), character()), ".is_scalar_character\\(path\\) is not TRUE")

    l <- list(
        hits = list(
            list(a = 1),
            list(b = 2),
            list(a = 3)
        )
    )
    x <- lol(l)
    expect_equal(
        lol_hits_lpull(x, "hits[*].a"),
        list(1, NULL, 3)
    )
    expect_equal(
        lol_hits_lpull(x, "hits[*]"),
        list(list(a = 1), list(b = 2), list(a = 3))
    )

    l <- list(
        hits = list(
            list(a = 1),
            list(b = 2, b = 3),
            list(a = 3)
        )
    )
    x <- lol(l)
    expect_equal(lol_hits_lpull(x, "hits[*]"), l$hits)
    expect_equal(
        lol_hits_lpull(x, "hits[*].b"),
        list(NULL, c(2, 3), NULL)
    )

})

test_that("'lol_hits_pull()' works", {
    expect_error(lol_hits_pull(), 'argument "x" is missing, with no default')
    expect_error(lol_hits_pull(list()), 'inherits\\(x, "lol"\\) is not TRUE')
    expect_error(lol_hits_pull(lol()), 'argument "path" is missing, with no default')
    expect_error(lol_hits_pull(lol(), character()), ".is_scalar_character\\(path\\) is not TRUE")

    l <- list(
        hits = list(
            list(a = 1),
            list(b = 2),
            list(a = 3)
        )
    )
    x <- lol(l)

    expect_equal(lol_hits_pull(x, "hits[*]"), c(1, 2, 3))
    expect_equal(lol_hits_pull(x, "hits[*].a"), c(1, NA, 3))
    expect_equal(lol_hits_pull(x, "hits[*].b"), c(NA, 2, NA))

})
