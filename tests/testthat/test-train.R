test.home <- abs(data.frame(c(rep(1,10),rep(2,10)), rep(seq(1:10),2)*60,sample(1:5,20, replace = TRUE),sample(1:5,20, replace = TRUE)))
test.away <- abs(data.frame(c(rep(1,10),rep(2,10)), rep(seq(1:10),2)*60,sample(1:5,20, replace = TRUE),sample(1:5,20, replace = TRUE)))

test.string <- data.frame(c(rep(1,10),rep(2,10)), rep(seq(1:10),2)*60, rep("a",20),sample(1:5,20, replace = TRUE))

test.neg <- data.frame(c(rep(1,10),rep(2,10)), rep(seq(1:10),2)*60,sample(1:5,20, replace = TRUE),sample(1:5,20, replace = TRUE)*-1)

test_that("return positive", {
  expect_gt(train(test.home, test.away)$ppm, 0)
})

test_that("return error non numeric", {
  expect_error(train(test.string, test.away))
})

test_that("return error different dimensions", {
  expect_error(train(test.home[,2:4], test.away))
})

test_that("return error neg values", {
  expect_error(train(test.neg, test.away))
})
