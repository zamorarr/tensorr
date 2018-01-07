context("dense")

# test data
dims <- c(2,2,2)
data <- array(c(1,0,0,0,1,0,0,0) , dim = dims)
Z <- dtensor(data)

test_that("dense tensor is initialized correctly given data", {
  expect_equal(Z@x, data, check.attributes = FALSE)
  expect_equal(dim(Z), dims)
  expect_true(is_dtensor(Z))
})

test_that("dense tensor throws errors for empty array", {
  expect_error(dtensor(numeric(0)))
})

test_that("dense tensor is considered a tensor", {
  expect_true(is_tensor(Z))
})

test_that("dense tensor has correct length", {
  expect_equal(length(Z), 8)
})

test_that("dense tensors show outputs", {
  expect_output(show(Z))
})

test_that("nzvals returns non-zero vals for dense tensor", {
  expect_equal(nzvals(Z), c(1,1))
})

test_that("nzsubs returns non-zero subscripts for dense tensor", {
  expect_equal(nzsubs(Z), matrix(c(1,1,1, 1,1,2), nrow = length(dims)))
})

test_that("zsubs returns zero subscripts for dense tensor", {
  expect_equal(zsubs(Z), array_index(c(2,3,4,6,7,8), dims))
})

test_that("nzsubs returns non-zero subscripts for dense tensor", {
  expect_equal(allsubs(Z), array_index(seq_along(Z), dims))
})




