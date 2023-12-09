test_that("returns positive pts and prob", {
  expect_gt(live(3.5, 5, 20, 70, 140)$exp.pts, 0)
  expect_gt(live(3.5, 5, 20, 70, 140)$prob, 0)
  expect_gt(live(3.5, 5, 20, 70, 140, 140)$prob, 0)
})

test_that("returns error on missing input", {
  expect_error(live(3.5, time = 20, threshold = 140))
})

test_that("returns error on bad input", {
  expect_error(live(3.5, -5, 20, 70, 140))
})
