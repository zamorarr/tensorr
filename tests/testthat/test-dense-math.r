context("dense-math")

# test data
dims <- c(3, 4, 2)
data <- array(1:24 , dim = dims)
Z <- dtensor(data)
U <- matrix(1:6, nrow = 2, ncol = 3)

test_that("norm of sparse tensor works with expected inputs", {
  expect_equal(norm(Z), 70)
})

test_that("inner product of sparse tensor works with expected inputs", {
  expect_equal(innerprod(Z,Z), 4900)
  expect_equal(sqrt(innerprod(Z,Z)), norm(Z))
})

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

test_that("dense tensor times vector works with expected inputs", {
  v <- 1:4
  res <- dtensor(array(c(70,80,90,190,200,210), c(3,2)))

  expect_equal(ttv(Z, v, 2), res)
})

test_that("order of dense tensor times vectors matters", {
  v1 <- 1:3
  v2 <- 11:14
  mode1 <- 1
  mode2 <- 2

  Y <- ttv(ttv(Z, v2, mode2), v1, mode1)
  #Y_wrong <- ttv(ttv(Z, v1, mode1), v2, mode2)
  Y_right <- ttv(ttv(Z, v1, mode1), v2, mode2 - 1)

  expect_equal(Y, Y_right)
  expect_error(ttv(ttv(Z, v1, mode1), v2, mode2))
})
