test_that("plot produced", {
  expect_is(plotCovid("San Diego County", "confirmed"), "ggplot")
})

test_that("invalid API field returns error", {
  expect_null(plotCovid("San Diego County", "comfirmed"))
})

test_that("no API field returns error", {
  expect_error(plotCovid("San Diego County"), "Variable to plot not specified")
})
