
# rm(list = ls()); library(testthat); library(arcstats)

context("Testing get_actigraph_SN()")

test_that("Test if ActigraphSN returns correct Actigraph serial number.", {

  expect_equal(
    get_actigraph_SN(system.file("extdata", extdata_fnames[1], package = "arcstats")),
    "TAS1D47140221"
    )

})
