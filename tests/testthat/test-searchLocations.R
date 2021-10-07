test_that("returns region name", {
  expect_match(searchLocations("East Asia & Pacific", -1), "East Asia & Pacific", fixed=T)
})

test_that("returns country name", {
  expect_match(searchLocations("Iraq", 0), "Iraq", fixed=T)
})

test_that("returns state name", {
  expect_match(searchLocations("New Hampshire", 1), "New Hampshire", fixed=T)
})

test_that("returns county name", {
  expect_match(searchLocations("Los Angeles County", 2), "Los Angeles County", fixed=T)
})

test_that("returns metro name", {
  expect_match(searchLocations("Kansas City, MO-KS", 1.5), "Kansas City, MO-KS", fixed=T)
})

test_that("no admin returns error", {
  expect_error(searchLocations("San Diego County"), "Administrative level not specified")
})

test_that("misspelling returns error", {
  expect_null(searchLocations("San Deigo", 2))
})


