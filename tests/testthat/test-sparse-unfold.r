context("sparse-unfold")

# test data
dims <- c(2,2,2)
subs <- matrix(c(1,1,1, 2,1,1, 1,1,2), nrow = length(dims))
vals <- c(10,100,20)
X <- sptensor(subs, vals, dims)

test_that("unfolding sparse tensor along mode 1 works with expected inputs", {
  i <- c(1,2,1)
  j <- c(1,1,3)
  newdims <- c(2,4)
  mode <- 1

  mat <- Matrix::sparseMatrix(i, j, x = vals, dims = newdims, giveCsparse = FALSE)
  expected <- unfolded_sptensor(mat, mode, dim(X))

  expect_equal(unfold(X, mode), expected)
})

test_that("unfolding sparse tensor along mode 2 works with expected inputs", {
  i <- c(1,1,1)
  j <- c(1,2,3)
  newdims <- c(2,4)
  mode <- 2

  mat <- Matrix::sparseMatrix(i, j, x = vals, dims = newdims, giveCsparse = FALSE)
  expected <- unfolded_sptensor(mat, mode, dim(X))

  expect_equal(unfold(X, mode), expected)
})

test_that("unfolding sparse tensor along mode 3 works with expected inputs", {
  i <- c(1,1,2)
  j <- c(1,2,1)
  newdims <- c(2,4)
  mode <- 3

  mat <- Matrix::sparseMatrix(i, j, x = vals, dims = newdims, giveCsparse = FALSE)
  expected <- unfolded_sptensor(mat, mode, dim(X))

  expect_equal(unfold(X, mode), expected)
})

test_that("refolding a folded tensor returns original tensor", {
  expect_equal(refold(unfold(X, 1)), X)
  expect_equal(refold(unfold(X, 2)), X)
  expect_equal(refold(unfold(X, 3)), X)
})

test_that("unfolded sparse tensor shows outputs", {
  expect_output(show(unfold(X, 1)))
})

test_that("refold_indices returns correct size results", {
  dim <- c(2,2,2,2)

  i1 <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)
  j1 <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

  actual <- refold_indices(i1, j1, 1, dim)
  expect_identical(nrow(actual), length(dim))
  expect_identical(ncol(actual), length(i1))

  i2 <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)
  j2 <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

  actual <- refold_indices(i2, j2, 2, c(2,2,2,2))
  expect_identical(nrow(actual), length(dim))
  expect_identical(ncol(actual), length(i2))
})
