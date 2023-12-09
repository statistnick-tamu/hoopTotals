test_that("returns error on missing input", {
  expect_error(plotTotal(3.5, 5, threshold = 140))
})

test_that("returns error on bad input", {
  expect_error(plotTotal(3.5, -5, 70, 140))
})
