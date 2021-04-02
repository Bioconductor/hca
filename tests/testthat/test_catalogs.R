test_that("catalogs are as expected", {
    current_json <- .hca_GET("/index/catalogs")
    current_catalogs <- names(current_json[["content"]][["catalogs"]])
    expect_setequal(catalogs(), current_catalogs)
})
