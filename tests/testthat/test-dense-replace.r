context("dense-replace")

# test data
dims <- c(2,2,2)
data <- array(c(10,0,0,0,20,0,0,0), dims)
Z <- dtensor(data)

test_that("empty replacement returns tensor filled with value", {
  arr <- array(3, dims)
  expect_equal({Z[] <- 3; Z}, dtensor(arr))
})

test_that("replacement using linear indexing works", {
  vec1 <- c(1,5)
  vec2 <- 1
  vec3 <- 1:2
  vec4 <- 2
  vec5 <- 100

  arr1 <- array(c(-10,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[vec1] <- c(-10,-20); Z}, dtensor(arr1))

  arr2 <- array(c(999,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[vec2] <- 999; Z}, dtensor(arr2))

  arr3 <- array(c(888,999,0,0,-20,0,0,0), dims)
  expect_equal({Z[vec3] <- c(888,999); Z}, dtensor(arr3))

  arr4 <- array(c(888,777,0,0,-20,0,0,0), dims)
  expect_equal({Z[vec4] <- 777; Z}, dtensor(arr4))
})

test_that("an out of bounds linear replacement throws error", {
  expect_error(Z[vec5] <- 999)
})

test_that("numeric matrix indexing of a tensor works", {
  mat1 <- matrix(c(1,1,1, 1,1,2), nrow = 3)
  mat2 <- matrix(c(1,1,1), nrow = 3)
  mat3 <- matrix(c(1,1,1, 2,2,2), nrow = 3)
  mat4 <- matrix(c(3,3,3), nrow = 3)

  arr1 <- array(c(-10,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[mat1] <- c(-10, -20); Z}, dtensor(arr1))

  arr2 <- array(c(100,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[mat2] <- 100; Z}, dtensor(arr2))

  arr3 <- array(c(20,0,0,0,-20,0,0,30), dims)
  expect_equal({Z[mat3] <- c(20, 30); Z}, dtensor(arr3))
})

test_that("an out of bounds matrix index replacement throws error", {
  expect_error(Z[mat4] <- 500)
})

test_that("replacement using list of numerics works", {
  list1 <- list(c(1,1,1), c(1,1,2))
  list2 <- list(c(1,1,1))
  list3 <- list(c(1,1,1), c(2,2,2))
  list4 <- list(c(3,3,3))

  arr1 <- array(c(-10,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[list1] <- c(-10, -20); Z}, dtensor(arr1))

  arr2 <- array(c(999,0,0,0,-20,0,0,0), dims)
  expect_equal({Z[list2] <- 999; Z}, dtensor(arr2))

  arr3 <- array(c(888,0,0,0,-20,0,0,999), dims)
  expect_equal({Z[list3] <- c(888, 999); Z}, dtensor(arr3))
})

test_that("an out of bounds list of numerics replacement throws error", {
  expect_error(Z[list4] <- 0)
})

test_that("replacement using multiple args works", {

  arr1 <- array(c(-10,0,0,0,20,0,0,0), dims)
  expect_equal({Z[1,1,1] <- -10; Z}, dtensor(arr1))

  arr2 <- array(c(-10,0,20,0,20,0,0,0), dims)
  expect_equal({Z[1,2,1] <- 20; Z}, dtensor(arr2))
})

test_that("an out of bounds multiple arg replacement throws error", {
  expect_error(Z[1,2,3] <- 999)
})

test_that("replacement using range/missing indexes works", {

  arr1 <- array(c(1,0,2,0,3,0,4,0), dims)
  expect_equal({Z[1,,] <- c(1,2,3,4); Z}, dtensor(arr1))

  arr2 <- array(c(50,0,2,0,60,0,4,0), dims)
  expect_equal({Z[1,1,] <- c(50,60); Z}, dtensor(arr2))

  arr3 <- array(c(50,0,2,0,25,35,45,55), dims)
  expect_equal({Z[,,2] <- c(25,35,45,55); Z}, dtensor(arr3))
})

test_that("an out of bounds range replacement throws error", {
  expect_error(Z[,,3] <- c(1,2,3,4))
})
