test_that("catalogs are as expected", {
    expect_true(setequal(catalogs(),
                         c("dcp1", "it1", "dcp2", "it2", "dcp3", "it3")))
})
