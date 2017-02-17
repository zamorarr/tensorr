context("dense-ops")

# test data
dims <- c(2,2,2)
arr <- array(c(10.6,0,0,0,20,0,0,0), dims)
arr2 <- array(c(0,0,0,30,40,0,0,0),dims)
Z <- dtensor(arr)
Z2 <- dtensor(arr2)

test_that("dense tensor Arithmitic works for expected inputs", {
  expect_equal(Z + Z2, dtensor(array(c(10.6,0,0,30,60,0,0,0), dims)))
  expect_equal(Z - Z2, dtensor(array(c(10.6,0,0,-30,-20,0,0,0), dims)))
  expect_equal(Z * Z2, dtensor(array(c(0,0,0,0,800,0,0,0), dims)))
  expect_equal(Z ^ 2, dtensor(array(c(10.6^2,0,0,0,400,0,0,0), dims)))
  expect_equal(Z %% 15, dtensor(array(c(10.6,0,0,0,5,0,0,0), dims)))
  expect_equal(Z %/% 15, dtensor(array(c(0,0,0,0,1,0,0,0), dims)))
  expect_equal(Z / 15, dtensor(array(c(10.6/15,0,0,0,4/3,0,0,0), dims)))

  expect_equal(-Z, 0 - Z)
  expect_equal(Z + 0, Z)
})

test_that("dense tensor Compare works for expected inputs", {
  expect_equal(Z == Z2, dtensor(array(c(F,T,T,F,F,T,T,T), dims)))
  expect_equal(Z > Z2, dtensor(array(c(T,F,F,F,F,F,F,F), dims)))
  expect_equal(Z < Z2, dtensor(array(c(F,F,F,T,T,F,F,F), dims)))
  expect_equal(Z != Z2, dtensor(array(c(T,F,F,T,T,F,F,F), dims)))
  expect_equal(Z <= Z2, dtensor(array(c(F,T,T,T,T,T,T,T), dims)))
  expect_equal(Z >= Z2, dtensor(array(c(T,T,T,F,F,T,T,T), dims)))
})

test_that("dense tensor Logic works for expected inputs", {
  expect_equal(Z & Z2, dtensor(array(c(F,F,F,F,T,F,F,F), dims)))
  expect_equal(Z | Z2, dtensor(array(c(T,F,F,T,T,F,F,F), dims)))
})

test_that("dense tensor Math works for expected inputs", {
  expect_equal(abs(-Z), Z)
  expect_equal(sqrt(Z^2), Z)
  expect_equal(round(Z), dtensor(array(c(11,0,0,0,20,0,0,0), dims)))
})

test_that("dense tensor Summary works for expected inputs", {
  expect_equal(max(Z), 20)
  expect_equal(sum(Z), 30.6)
})

test_that("dense tensor Complex works for expected inputs", {
  arr3 <- array(complex(8, real = 1:8, imaginary = 9:16), dims)
  Z3 <- dtensor(arr3)
  expect_equal(Re(Z3), dtensor(array(1:8, dims)))
  expect_equal(Im(Z3), dtensor(array(9:16, dims)))
})

test_that("Ops on dense tensor and sparse tensor produce a dense tensor", {
  X <- as_sptensor(Z)
  expect_equal(X + Z2, dtensor(array(c(10.6,0,0,30,60,0,0,0), dims)))
  expect_equal(Z2 - X, dtensor(array(c(-10.6,0,0,30,20,0,0,0), dims)))
})
