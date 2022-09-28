test_that("catalogs are as expected", {
    current_json <- .hca_GET("/index/catalogs")
    current_catalogs <- names(current_json[["content"]][["catalogs"]])
    expect_setequal(catalogs(), current_catalogs)
})

test_that("user can override a catalog", {
    catalogs <- catalogs()
    choice <- tail(catalogs, 1L)
    expect_identical(choice, head(catalogs(choice), 1L))
    expect_error(catalogs("foo"))
    ## restore catalogs to default with 'NULL' argument
    expect_identical(catalogs, catalogs(NULL))
})
