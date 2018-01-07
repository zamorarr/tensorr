context("sparse-extract-dimnames")

# test data
subs <- matrix(as.integer(c(1,1,1,1, 1,1,1,2)), c(4,2))
vals <- c(10,20)
dims <- c(2,2,2,2)
X <- sptensor(subs, vals, dims)

test_that("dimnames set correctly for sparse tensor", {
  newnames <-list(LETTERS[1:2], letters[1:2], month.abb[1:2], month.name[1:2])
  dimnames(X) <- newnames
  expect_identical(dimnames(X), newnames)
})

test_that("cannot set dimnames with incorrect dimensions", {
  badnames1 <-list(LETTERS[1:2], letters[1:2], month.abb[1:2])
  badnames2 <-list(LETTERS[1:2], letters[1:2], month.abb[1:2], month.name[1:10])

  expect_error(dimnames(X) <- badnames1)
  expect_error(dimnames(X) <- badnames2)
})

test_that("indexing by a a non-existant dimname throws error", {
  expect_error(X[,,"Dec",])
})

test_that("multiple dimnames args index tensor", {
  newnames <-list(LETTERS[1:2], letters[1:2], month.abb[1:2], month.name[1:2])
  dimnames(X) <- newnames

  expect_equal(X["A","a","Jan","January"], 10)
  expect_equal(X["A","a","Jan","February"], 20)
  expect_equal(X["B","a","Jan","February"], 0)
})

test_that("dimnames are subsetted correctly after extracting a subtensor", {
  newnames <-list(LETTERS[1:2], letters[1:2], month.abb[1:2], month.name[1:2])
  dimnames(X) <- newnames

  actual <- dimnames(X[1,1,1,])
  expected <- list("A", "a", "Jan", c("January", "February"))
  expect_identical(actual, expected)

  actual <- dimnames(X[1,1,c(2,2),])
  expected <- list("A", "a", c("Feb", "Feb"), c("January", "February"))
  expect_identical(actual, expected)
})
