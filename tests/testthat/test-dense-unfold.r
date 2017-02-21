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
})

test_that("unfolded dense tensor shows outputs", {
  expect_output(show(unfold(Z, 1)))
})
