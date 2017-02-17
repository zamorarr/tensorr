context("dense-convert")

# test data
dims <- c(2,2,2)
arr <- array(c(1,0,0,0,1,0,0,0), dims)
X <- sptensor(subs = matrix(c(1,1,1, 1,1,2), nrow = length(dims)),
              vals = c(1,1),
              dims = dims)
Z <- dtensor(arr)

test_that("sparse tensor converts to dense tensor", {
  expect_equal(as_dtensor(X), Z)
})

test_that("array converts to dense tensor", {
  expect_equal(as_dtensor(arr), Z)
})

test_that("as.vector works", {
  expect_equal(as.vector(Z), c(1,0,0,0,1,0,0,0))
})
