test_that("plot produced", {
  expect_is(plotEpiData("San Diego County", "confirmed"), "ggplot")
})

test_that("invalid API field returns error", {
  expect_null(plotEpiData("San Diego County", "comfirmed"))
})

test_that("no API field returns error", {
  expect_error(plotEpiData("San Diego County"), "Variable to plot not specified")
})
