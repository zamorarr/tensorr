context("dense-math")

# test data
dims <- c(3, 4, 2)
data <- array(1:24 , dim = dims)
Z <- dtensor(data)
U <- matrix(1:6, nrow = 2, ncol = 3)

test_that("dense tensor times matrix works with expected inputs", {
  res <- array(c(22,28,49,64,76,100,103,136,
                 130,172,157,208,184,244,211,280), c(2,4,2))
  res <- dtensor(res)

  expect_equal(ttm(Z, U, 1), res)
})

test_that("order of dense tensor times matrices doesn't matter", {
  W <- matrix(1:8, nrow = 2, ncol = 4)
  Y1 <- ttm(ttm(Z, U, 1), W, 2)
  Y2 <- ttm(ttm(Z, W, 2), U, 1)

  expect_equal(Y1, Y2)
})
