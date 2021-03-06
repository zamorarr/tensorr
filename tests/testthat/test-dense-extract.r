context("dense-extract")

# test data
dims <- c(2,2,2)
data <- array(c(10,0,0,0,20,0,0,0), dims)
Z <- dtensor(data)

test_that("empty extract returns original tensor", {
  expect_equal(Z[], Z)
})

test_that("linear indexing of a tensor works", {
  vec1 <- c(1,5)
  vec2 <- 1
  vec3 <- 1:2
  vec4 <- 2

  expect_equal(Z[vec1], c(10,20))
  expect_equal(Z[vec2], 10)
  expect_equal(Z[vec3], c(10,0))
  expect_equal(Z[vec4], 0)
})

test_that("an out of bounds linear index returns NA", {
  expect_equal(Z[100], NA_real_)
})

test_that("numeric matrix indexing of a tensor works", {
  mat1 <- matrix(c(1,1,1, 1,1,2), nrow = 3)
  mat2 <- matrix(c(1,1,1), nrow = 3)
  mat3 <- matrix(c(1,1,1, 2,2,2), nrow = 3)
  mat4 <- matrix(c(2,2,2), nrow = 3)

  expect_equal(Z[mat1], c(10,20))
  expect_equal(Z[mat2], 10)
  expect_equal(Z[mat3], c(10,0))
  expect_equal(Z[mat4], 0)
})

test_that("list of numerics indexes tensor", {
  list1 <- list(c(1,1,1), c(1,1,2))
  list2 <- list(c(1,1,1))
  list3 <- list(c(1,1,1), c(2,2,2))
  list4 <- list(c(2,2,2))

  expect_equal(Z[list1], c(10,20))
  expect_equal(Z[list2], 10)
  expect_equal(Z[list3], c(10,0))
  expect_equal(Z[list4], 0)
})

test_that("multiple args index tensor", {
  expect_equal(Z[1,1,1], 10)
  expect_equal(Z[1,2,1], 0)
})

test_that("an out of bounds multiple arg index returns an error", {
  expect_error(Z[1,2,3]) # Inconsident with sparse version?
})

test_that("range/missing indexes return a subtensor", {
  expect_equal(dim(Z[1,,]), c(1,2,2))
  expect_equal(dim(Z[1,1,]), c(1,1,2))
  expect_equal(dim(Z[,,2]), c(2,2,1))
  expect_equal(dim(Z[1,1:2,1,drop=TRUE]), c(2,1)) # Inconsistent with sp?

  subs2 <- matrix(integer(0L), nrow = 3)
  vals2 <- double(0L)
  expect_equal(Z[,2,], dtensor(array(rep(0, 4L), c(2,1,2))))
})


