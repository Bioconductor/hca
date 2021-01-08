test_that("'filters()' works", {

    ## empty object
    object <- filters()
    expect_equal(length(object), 0)
    expect_equal(.filters_filters(object), setNames(list(), character()))
    expect_equal(
        .filters_encoding(object),
        URLencode('{}', reserved = TRUE)
    )

    ## single filter
    object <- filters(organ = list(is = "pancreas"))
    expect_equal(length(object), 1L)
    expect_equal(.filters_filters(object), list(organ = list(is = "pancreas")))
    expect_equal(
        as.character(.filters_encoding(object)),
        URLencode('{"organ":{"is":["pancreas"]}}', reserved = TRUE)
    )

    ## single filter, multiple match
    object <- filters(fileFormat = list(is = c("fastq", "fastq.gz")))
    expect_equal(length(object), 1L)
    expect_equal(
        as.character(.filters_encoding(object)),
        URLencode('{"fileFormat":{"is":["fastq","fastq.gz"]}}', reserved = TRUE)
    )

    ## two filters
    object <- filters(
        organ = list(is = "pancreas"),
        genusSpecies = list(is = "Homo sapiens")
    )
    expect_equal(length(object), 2L)
    expect_equal(
        .filters_filters(object),
        list(
            organ = list(is = "pancreas"),
            genusSpecies = list(is = "Homo sapiens")
        )
    )
    expect_equal(
        as.character(.filters_encoding(object)),
        URLencode(
            '{"organ":{"is":["pancreas"]},"genusSpecies":{"is":["Homo sapiens"]}}',
            reserved = TRUE
        )
    )

})

test_that("'filters()' validates arguments", {

    ## not named lists
    expect_error(filters(organ = "bar"), ".*named lists")
    expect_error(filters(organ = c(is = "bar")), ".*named lists")
    ## invalid verbs
    expect_error(filters(organ = list(isnota = "bar")), ".*verbs must be")

    ## valid verbs: "is", "within", "contains", and "intersects"
    ## details don't need to be checked; these are satified in an
    ## earilier test
    class <- c("filters", "HCAccess")
    expect_s3_class(filters(organ = list(is = "bar")), class)
    expect_s3_class(filters(organ = list(within = "bar")), class)
    expect_s3_class(filters(organ = list(contains = "bar")), class)
    expect_s3_class(filters(organ = list(intersects = "bar")), class)

    ## FIXME invalid / valid nouns ("organ", "genusSpecies", ...)

})
