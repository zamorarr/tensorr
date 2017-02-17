context("sparse-convert")

# test data
dims <- c(2,2,2)
df <- data.frame(i = c(1,1), j = c(1,1), k = c(1,2), val = c(1,1))
X <- sptensor(subs = matrix(c(1,1,1, 1,1,2), nrow = length(dims)),
              vals = c(1,1),
              dims = dims)
arr <- array(c(1,0,0,0,1,0,0,0), dims)
Z <- dtensor(arr)

test_that("data frame converts to sptensor", {
  expect_equal(as_sptensor(df, dims = dims), X)
})

test_that("dense tensor converts to sparse tensor", {
  expect_equal(as_sptensor(Z), X)
})

test_that("as.vector works", {
  expect_equal(as.vector(X), c(1,0,0,0,1,0,0,0))
})
