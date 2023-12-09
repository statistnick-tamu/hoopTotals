test_that("positive points", {
  expect_gt(total(3.5,5), 0)
})

test_that("negative input error", {
  expect_error(total(-3.5,5))
})

test_that("bad line error", {
  expect_error(total(3.5,5, -150))
})
