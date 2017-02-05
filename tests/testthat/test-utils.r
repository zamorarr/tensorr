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
