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
