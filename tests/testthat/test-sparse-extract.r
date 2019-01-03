context("sparse-extract")

# test data
subs <- matrix(as.integer(c(1,1,1, 1,1,2)), c(3,2))
vals <- c(10,20)
dims <- c(2,2,2)
X <- sptensor(subs, vals, dims)

test_that("empty extract returns original tensor", {
  expect_equal(X[], X)
})

test_that("linear indexing of a tensor works", {
  vec1 <- c(1,5)
  vec2 <- 1
  vec3 <- 1:2
  vec4 <- 2
  vec5 <- 100

  expect_equal(X[vec1], c(10,20))
  expect_equal(X[vec2], 10)
  expect_equal(X[vec3], c(10,0))
  expect_equal(X[vec4], 0)
  expect_equal(X[vec5], NA_integer_)
})

test_that("an out of bounds linear index returns NA", {
  expect_equal(X[100], NA_real_)
})

test_that("numeric matrix indexing of a tensor works", {
  mat1 <- matrix(c(1,1,1, 1,1,2), nrow = 3)
  mat2 <- matrix(c(1,1,1), nrow = 3)
  mat3 <- matrix(c(1,1,1, 2,2,2), nrow = 3)
  mat4 <- matrix(c(2,2,2), nrow = 3)

  expect_equal(X[mat1], c(10,20))
  expect_equal(X[mat2], 10)
  expect_equal(X[mat3], c(10,0))
  expect_equal(X[mat4], 0)
})

test_that("list of numerics indexes tensor", {
  list1 <- list(c(1,1,1), c(1,1,2))
  list2 <- list(c(1,1,1))
  list3 <- list(c(1,1,1), c(2,2,2))
  list4 <- list(c(2,2,2))

  expect_equal(X[list1], c(10,20))
  expect_equal(X[list2], 10)
  expect_equal(X[list3], c(10,0))
  expect_equal(X[list4], 0)
})

test_that("multiple args index tensor", {
  expect_equal(X[1,1,1], 10)
  expect_equal(X[1,2,1], 0)
})

test_that("an out of bounds multiple arg index returns an NA", {
  expect_equal(X[1,2,3], NA_real_) # inconsistent with dense version?
})

test_that("range/missing indexes return a subtensor", {
  expect_equal(dim(X[1,,]), c(1,2,2))
  expect_equal(dim(X[1,1,]), c(1,1,2))
  expect_equal(dim(X[,,2]), c(2,2,1))
  expect_equal(dim(X[1,1:2,1,drop=TRUE]), c(2))

  subs2 <- matrix(integer(0L), nrow = 3)
  vals2 <- double(0L)
  expect_equal(X[,2,], sptensor(subs2, vals2, c(2,1,2)))
})

test_that("indexing with a vector and missing indices works", {
  X2 <- X[,,c(1,1)]

  expect_equal(dim(X2), c(2,2,2))
  expect_equal(nzvals(X2), c(10,10))
  expect_equal(nzsubs(X2), matrix(as.integer(c(1,1,1,1,1,2)), nrow = 3L, ncol = 2L))#, check.attributes = FALSE)
})

test_that("indexing with negative subscripts works", {
  X2 <- X[,,-1]
  expect_identical(X2, X[,,2])
})

test_that("indexing with a repeated value works", {
  Z <- dtensor(array(1:12, dim = c(2,3,2)))
  Y <- as_sptensor(Z)

  Y2 <- Y[1,c(2,1),c(1,2,1)]
  expect_equal(dim(Y2), c(1,2,3))
  expect_equal(nzvals(Y2), c(3,1,9,7,3,1))
  expect_equal(nzsubs(Y2), matrix(c(1,1,1, 1,2,1, 1,1,2, 1,2,2, 1,1,3, 1,2,3), nrow = 3, ncol = 6))
})
