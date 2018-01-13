context("utils")

test_that("vec_index is correct", {
  dims <- c(3,4,2)
  expect_equal(vec_index(c(1,1,1), dims), 1)
  expect_equal(vec_index(c(2,3,1), dims), 8)
  expect_equal(vec_index(c(1,3,2), dims), 19)
  expect_equal(vec_index(c(3,4,2), dims), 24)

  expect_equal(vec_index(matrix(c(1,1,1, 2,3,1, 3,4,2), nrow = 3), dims), c(1,8,24))
  expect_equal(vec_index(list(c(1,1,1), c(2,3,1), c(3,4,2)), dims), c(1,8,24))
})

test_that("array_index is correct", {
  dims <- c(3,4,2)
  expect_equal(array_index(1, dims), matrix(c(1,1,1), nrow = 3))
  expect_equal(array_index(8, dims), matrix(c(2,3,1), nrow = 3))
  expect_equal(array_index(19, dims), matrix(c(1,3,2), nrow = 3))
  expect_equal(array_index(24, dims), matrix(c(3,4,2), nrow = 3))

  mat <- matrix(c(1,1,1,2,3,1,3,4,2), nrow = 3)
  expect_equal(array_index(c(1,8,24), dims), mat)
})

test_that("vec_index and array_index are inverses of each other", {
  dims <- c(2,3,4)
  i <- 1:24

  expect_equal(vec_index(array_index(i, dims), dims), i)
})

test_that("fill missing indices is correct", {
  expect_equal(fill_missing_indices(1, 10), 1)
  expect_equal(fill_missing_indices(NULL, 10), 1:10)
  expect_equal(fill_missing_indices(-5, 10), c(1:4, 6:10))
})
