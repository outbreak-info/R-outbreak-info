test_that("returns region ISO3", {
  expect_match(getISO3("East Asia & Pacific"), "East_Asia___Pacific", fixed=T)
})

test_that("returns country ISO3", {
  expect_match(getISO3("Iraq"), "IRQ", fixed=T)
})

test_that("returns state ISO3", {
  expect_match(getISO3("New Hampshire"), "USA_US-NH", fixed=T)
})

test_that("returns county ISO3", {
  expect_match(getISO3("Los Angeles County"), "USA_US-CA_06037", fixed=T)
})

test_that("returns metro ISO3", {
  expect_match(getISO3("Kansas City"), "CITY_US-MO_KC", fixed=T)
})
