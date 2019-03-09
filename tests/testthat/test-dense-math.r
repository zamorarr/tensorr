context("dense-math")

# test data
dims <- c(3, 4, 2)
data <- array(1:24 , dim = dims)
Z <- dtensor(data)
U <- matrix(1:6, nrow = 2, ncol = 3)

test_that("norm of dense tensor works with expected inputs", {
  expect_equal(norm(Z), 70)
})

test_that("inner product of dense tensor works with expected inputs", {
  expect_equal(innerprod(Z,Z), 4900)
  expect_equal(sqrt(innerprod(Z,Z)), norm(Z))
})

test_that("outer product of dense tensor works with expected inputs", {
  res <- outer(Z@x, Z@x)
  expect_equal(outerprod(Z,Z), dtensor(res))
  expect_equal(ttt(Z,Z), dtensor(res))
})

test_that("outer product of dense tensor works with sparse tensor inputs", {
  Y <- as_sptensor(Z)
  res <- outer(Z@x, Z@x)
  expect_equal(outerprod(Z,Y), dtensor(res))
  expect_equal(outerprod(Y,Z), dtensor(res))
})

test_that("dense tensor times matrix works with expected inputs", {
  res <- array(c(22,28,49,64,76,100,103,136,
                 130,172,157,208,184,244,211,280), c(2,4,2))
  res <- dtensor(res)

  expect_equal(ttm(Z, U, 1), res)
})

test_that("symmetric dense tensor time matrix retains symmetry", {
  # example from https://github.com/zamorarr/tensorr/issues/17
  x <- c(-8.2, 1.1, 1.1, -0.2,
         -3.2, -0.2, -0.2, 0.1,
         -8.2, 1.1, 1.1, -0.2,
         -3.2, -0.2, -0.2, 0.1)
  x <- array(x, dim = c(2,2,2,2))
  X <- dtensor(x)
  A <- matrix(c(-1.7, 2.9, 0.4, 0.0), nrow = 2)
  actual <- ttm(ttm(X,A,1),A,2)

  expected <- c(-25.226, 41.702, 41.702, -68.962,
                -8.960, 15.544, 15.544, -26.912,
                -25.226, 41.702, 41.702, -68.962,
                -8.960, 15.544, 15.544, -26.912
                )
  expected <- dtensor(array(expected, dim = c(2,2,2,2)))

  expect_equal(actual@x, expected@x)
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
