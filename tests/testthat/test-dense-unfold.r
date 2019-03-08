context("dense-unfold")

# test data
dims <- c(2,2,2)
data <- array(c(10,100,0,0,20,0,0,0) , dim = dims)
Z <- dtensor(data)


test_that("unfolding dense tensor along mode 1 works with expected inputs", {
  mode <- 1
  vals <- c(10,100,0,0,20,0,0,0)
  mat <- Matrix::Matrix(vals, nrow = 2, ncol = 4, sparse = FALSE)
  expected <- unfolded_dtensor(mat, mode, dim(Z))

  expect_equal(unfold(Z, mode), expected)
})

test_that("unfolding sparse tensor along mode 2 works with expected inputs", {
  mode <- 2
  vals <- c(10,0,100,0,20,0,0,0)
  mat <- Matrix::Matrix(vals, nrow = 2, ncol = 4, sparse = FALSE)
  expected <- unfolded_dtensor(mat, mode, dim(Z))

  expect_equal(unfold(Z, mode), expected)

  expect_equal(unfold(Z, mode), expected)
})

test_that("unfolding sparse tensor along mode 3 works with expected inputs", {
  mode <- 3
  vals <- c(10,20,100,0,0,0,0,0)
  mat <- Matrix::Matrix(vals, nrow = 2, ncol = 4, sparse = FALSE)
  expected <- unfolded_dtensor(mat, mode, dim(Z))

  expect_equal(unfold(Z, mode), expected)
})

test_that("refolding a folded tensor returns original tensor", {
  expect_equal(refold(unfold(Z, 1)), Z)
  expect_equal(refold(unfold(Z, 2)), Z)
  expect_equal(refold(unfold(Z, 3)), Z)

  z2 <- c(0,8,16,2,10,18,4,12,20,6,14,22,1,9,17,3,11,19,5,13,21,7,15,23)
  z2 <- array(z2, dim = c(3,4,2))
  Z2 <- dtensor(z2)

  expect_equal(refold(unfold(Z2, 1)), Z2)
  expect_equal(refold(unfold(Z2, 2)), Z2)
  expect_equal(refold(unfold(Z2, 3)), Z2)

  z3 <- c(-8.2, 1.1, 1.1, -0.2, -3.2, -0.2, -0.2, 0.1, -8.2, 1.1, 1.1, -0.2, -3.2, -0.2, -0.2, 0.1)
  z3 <- array(z3, dim = c(2,2,2,2))
  Z3 <- dtensor(z3)

  expect_equal(refold(unfold(Z3, 1)), Z3)
  expect_equal(refold(unfold(Z3, 2)), Z3)
  expect_equal(refold(unfold(Z3, 3)), Z3)
  expect_equal(refold(unfold(Z3, 4)), Z3)
})

test_that("unfolded dense tensor shows outputs", {
  expect_output(show(unfold(Z, 1)))
})
