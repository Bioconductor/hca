test_that("'.is_scalar()' works", {
    ## scalar
    expect_false(.is_scalar(logical()))
    expect_true(.is_scalar(logical(1)))
    expect_false(.is_scalar(logical(2)))

    ## NA values
    expect_false(.is_scalar(NA))
    expect_true(.is_scalar(NA, na.ok = TRUE))
})

test_that("'.is_scalar_*()' works", {
    expect_true(.is_scalar_character("foo"))
    expect_true(.is_scalar_logical(TRUE))
    expect_true(.is_scalar_integer(1L))
    expect_true(.is_scalar_numeric(1))
})

test_that("'.is_scalar_character()' nzchar works", {
    expect_true(.is_scalar_character(""))
})

test_that("'.wrap_lines()' wraps lines", {
    oopt = options(width = 15)
    on.exit(options(oopt))

    x <- "one word or another"
    expected <- "one word or\n  another"
    expect_identical(.wrap_lines(x), expected)
    expect_identical(.wrap_lines(strsplit(x, " ")[[1]]), expected)

    ## leading / trailing whitespace trimmed
    expect_identical(.wrap_lines(c("one word or ", "another")), expected)
    expect_identical(.wrap_lines(c("one word or ", " another ")), expected)

    ## edge cases
    expect_identical(.wrap_lines(character()), "")
    expect_identical(.wrap_lines(""), "")
    expect_identical(.wrap_lines(c("one word or", "", "another")), expected)
})
