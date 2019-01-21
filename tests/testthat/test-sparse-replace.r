context("sparse-replace")

# test data
subs <- matrix(as.integer(c(1,1,1, 1,1,2)), c(3,2))
vals <- c(10,20)
dims <- c(2,2,2)
X <- sptensor(subs, vals, dims)

test_that("empty replacement returns an error", {
  expect_error(X[] <- 3)
})

test_that("replacement using linear indexing works", {
  vec1 <- c(1,5)
  vec2 <- 1
  vec3 <- 1:2
  vec4 <- 2
  vec5 <- 100

  vals1 <- c(-10,-20)
  expect_equal({X[vec1] <- vals1; X}, sptensor(subs, vals1, dims))

  subs2 <- matrix(c(1,1,1, 1,1,2), nrow = length(dims))
  vals2 <- c(999, -20)
  expect_equal({X[vec2] <- 999; X}, sptensor(subs2, vals2, dims))

  subs3 <- matrix(c(1,1,1, 2,1,1, 1,1,2), nrow = length(dims))
  vals3 <- c(888, 999, -20)
  expect_equal({X[vec3] <- c(888,999); X}, sptensor(subs3, vals3, dims))

  subs4 <- matrix(c(1,1,1, 2,1,1, 1,1,2), nrow = length(dims))
  vals4 <- c(888, 777, -20)
  expect_equal({X[vec4] <- 777; X}, sptensor(subs4, vals4, dims))
})

test_that("an out of bounds linear replacement throws error", {
  expect_error(X[vec5] <- 999)
})

test_that("numeric matrix indexing of a tensor works", {
  mat1 <- matrix(c(1,1,1, 1,1,2), nrow = 3)
  mat2 <- matrix(c(1,1,1), nrow = 3)
  mat3 <- matrix(c(1,1,1, 2,2,2), nrow = 3)
  mat4 <- matrix(c(3,3,3), nrow = 3)

  vals1 <- c(-10,-20)
  expect_equal({X[mat1] <- c(-10, -20); X}, sptensor(subs, vals1, dims))

  subs2 <- matrix(c(1,1,1, 1,1,2), nrow = length(dims))
  vals2 <- c(100, -20)
  expect_equal({X[mat2] <- 100; X}, sptensor(subs2, vals2, dims))

  subs3 <- array_index(c(1,5,8), dims)
  vals3 <- c(20,-20,30)
  expect_equal({X[mat3] <- c(20, 30); X}, sptensor(subs3, vals3, dims))
})

test_that("an out of bounds matrix index replacement throws error", {
  expect_error(X[mat4] <- 500)
})

test_that("replacement using list of numerics works", {
  list1 <- list(c(1,1,1), c(1,1,2))
  list2 <- list(c(1,1,1))
  list3 <- list(c(1,1,1), c(2,2,2))
  list4 <- list(c(3,3,3))

  subs1 <- array_index(c(1,5), dims)
  vals1 <- c(-10, -20)
  expect_equal({X[list1] <- c(-10, -20); X}, sptensor(subs1, vals1, dims))

  subs2 <- array_index(c(1,5), dims)
  vals2 <- c(999, -20)
  expect_equal({X[list2] <- 999; X}, sptensor(subs2, vals2, dims))

  subs3 <- array_index(c(1,5,8), dims)
  vals3 <- c(888, -20, 999)
  expect_equal({X[list3] <- c(888, 999); X}, sptensor(subs3, vals3, dims))
})

test_that("an out of bounds list of numerics replacement throws error", {
  expect_error(X[list4] <- 0)
})

test_that("replacement using multiple args works", {

  subs1 <- array_index(c(1,5), dims)
  vals1 <- c(-10, 20)
  expect_equal({X[1,1,1] <- -10; X}, sptensor(subs1, vals1, dims))

  subs2 <- array_index(c(1,3,5), dims)
  vals2 <- c(-10, 20, 20)
  expect_equal({X[1,2,1] <- 20; X}, sptensor(subs2, vals2, dims))
})

test_that("an out of bounds multiple arg replacement throws error", {
  expect_error(X[1,2,3] <- 999)
})

test_that("replacement using range/missing indexes works", {

  subs1 <- array_index(c(1,3,5,7), dims)
  vals1 <- c(1,2,3,4)
  expect_equal({X[1,,] <- c(1,2,3,4); X}, sptensor(subs1, vals1, dims))

  subs2 <- array_index(c(1,3,5,7), dims)
  vals2 <- c(50, 2, 60, 4)
  expect_equal({X[1,1,] <- c(50,60); X}, sptensor(subs2, vals2, dims))

  subs3 <- array_index(c(1,3,5,6,7,8), dims)
  vals3 <- c(50, 2, 25, 35, 45, 55)
  expect_equal({X[,,2] <- c(25,35,45,55); X}, sptensor(subs3, vals3, dims))

  subs4 <- array_index(c(1,5,6), dims)
  vals4 <- c(50, 25, 35)
  expect_equal({X[,2,] <- 0; X}, sptensor(subs4, vals4, dims))
})

test_that("an out of bounds range replacement throws error", {
  expect_error(X[,,3] <- c(1,2,3,4))
})

test_that("replacement using dimnames works", {
  dimnames(X) <- list(
    c("A", "B"), c("a", "b"), c("Jan", "Feb")
  )

  newX <- sptensor(subs,  c(100, 20), dims)
  dimnames(newX) <- dimnames(X)

  expect_equal({X["A", "a", "Jan"] <- 100; X}, newX)
})

test_that("dimnames carry over after replacement", {
  dimnames(X) <- list(
    c("A", "B"), c("a", "b"), c("Jan", "Feb")
  )

  # save dimnames
  oldnames <- dimnames(X)

  # replace value
  X["A", "a", "Jan"] <- 100

  # ensure dimnames didn't change
  expect_identical(dimnames(X), oldnames)
})

test_that("replacement using missing dimnames works", {
  dimnames(X) <- list(
    c("A", "B"), c("a", "b"), c("Jan", "Feb")
  )

  subs1 <- array_index(c(1,3,5,7), dims)
  vals1 <- c(1,2,3,4)
  X1 <- sptensor(subs1, vals1, dims)
  dimnames(X1) <- dimnames(X)
  expect_equal({X["A",,] <- c(1,2,3,4); X}, X1)

  subs2 <- array_index(c(1,3,5,7), dims)
  vals2 <- c(50, 2, 60, 4)
  X2 <- sptensor(subs2, vals2, dims)
  dimnames(X2) <- dimnames(X)
  expect_equal({X["A","a",] <- c(50,60); X}, X2)

  subs3 <- array_index(c(1,3,5,6,7,8), dims)
  vals3 <- c(50, 2, 25, 35, 45, 55)
  X3 <- sptensor(subs3, vals3, dims)
  dimnames(X3) <- dimnames(X)
  expect_equal({X[,,"Feb"] <- c(25,35,45,55); X}, X3)

  subs4 <- array_index(c(1,5,6), dims)
  vals4 <- c(50, 25, 35)
  X4 <- sptensor(subs4, vals4, dims)
  dimnames(X4) <- dimnames(X)
  expect_equal({X[,"b",] <- 0; X}, X4)
})

test_that("indexing with a single dimname throws error", {
  expect_error(X["Jan"] <- 3)
})
