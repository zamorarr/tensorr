context("init")

subs <- matrix(c(1,1,1, 1,1,2), c(3,2))
vals <- c(10,20)
dims <- c(2,2,2)

data <- array(data = c(1,0,0,0,1,0,0,0) , dim = dims)

X <- sptensor(subs, vals, dims)
Z <- dtensor(data)

test_that("sparse tensor is initialized given subs, vals, dims", {
  expect_equal(X@subs, subs)
  expect_equal(X@vals, vals)
  expect_equal(X@dims, dims)

  expect_true(is_sptensor(X))
})

test_that("sparse tensor is initialized when vals missing", {
  X <- sptensor(subs, dims = dims)

  expect_equal(X@subs, subs)
  expect_equal(X@vals, c(1,1))
  expect_equal(X@dims, dims)
})

test_that("sparse tensor is initialized with a list of subscripts", {
  subslist <- list(c(1,1,1), c(1,1,2))
  X <- sptensor(subslist, vals, dims = dims)

  expect_equal(X@subs, subs)
  expect_equal(X@vals, vals)
  expect_equal(X@dims, dims)
})

test_that("sparse tensor is initialized correctly", {
  expect_equal(X@subs, subs)
  expect_equal(X@vals, vals)
  expect_equal(X@dims, dims)
})

test_that("sparse tensor throws errors for empty subs, vals, or dims", {
  expect_error(sptensor(numeric(0), vals, dims))
  expect_error(sptensor(subs, double(0), dims))
  expect_error(sptensor(subs, vals, integer(0)))
})

test_that("dense tensor is initialized correctly given data", {
  expect_equal(Z@x, data)
  expect_equal(dim(Z), dims)
  expect_true(is_dtensor(Z))
})

test_that("dense tensor throws errors for empty array", {
  expect_error(dtensor(numeric(0)))
})

test_that("sparse and dense tensors are considered tensors", {
  expect_true(is_tensor(Z))
  expect_true(is_tensor(X))

})

test_that("tensors have correct length", {
  expect_equal(length(X), 8)
  expect_equal(length(Z), 8)
})

test_that("tensors show outputs", {
  X <- sptensor(subs, vals, dims)
  Z <- dtensor(data)
  expect_output(show(X))
  expect_output(show(Z))
})
