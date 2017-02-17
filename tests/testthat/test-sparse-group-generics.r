context("sparse-group-generics")

# test data
dims <- c(2,2,2)
subs <-matrix(c(1,1,1, 1,1,2), nrow = length(dims))
vals <- c(10.6, 20)
subs2 <- matrix(c(2,2,1, 1,1,2), nrow = length(dims))
vals2 <- c(30, 40)

X <- sptensor(subs, vals, dims)
X2 <- sptensor(subs2, vals2, dims)

test_that("sparse tensor addition works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 2,2,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(10.6, 30, 60)
  expect_equal(X + X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor subtraction works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 2,2,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(10.6, -30, -20)
  expect_equal(X - X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor multiplication works for expected inputs", {
  subs_expected <- matrix(c(1,1,2), nrow = length(dims))
  vals_expected <- 800
  expect_equal(X * X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor power works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(10.6, 20)^2
  expect_equal(X ^ 2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor mod works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(10.6, 20) %% 15
  expect_equal(X %% 15, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor integer division works for expected inputs", {
  subs_expected <- matrix(c(1,1,2), nrow = length(dims))
  vals_expected <- 1
  expect_equal(X %/% 15, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor division works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(10.6, 20) / 15
  expect_equal(X / 15, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor ops works for missing or zero input", {
  expect_equal(-X, 0 - X)
  expect_equal(X + 0, X)
})

test_that("sparse tensor ops works for scalar input", {
  subs_expected <- allsubs(X)
  vals_expected <- c(10.6,0,0,0,20,0,0,0) + 3
  expect_equal(X + 3, sptensor(subs_expected, vals_expected, dims))
  expect_equal(3 + X, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor equals comparison works for expected inputs", {
  subs_expected <- allsubs(X)
  vals_expected <- c(F,T,T,F,F,T,T,T)
  expect_equal(X == X2, sptensor(subs_expected, vals_expected, dims))
  expect_warning(X == X2)
})

test_that("sparse tensor greater-than comparison works for expected inputs", {
  subs_expected <- matrix(c(1,1,1), nrow = length(dims))
  vals_expected <- TRUE
  expect_equal(X > X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor less-than comparison works for expected inputs", {
  subs_expected <- matrix(c(2,2,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(TRUE, TRUE)
  expect_equal(X < X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor not-equals comparison works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 2,2,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(TRUE, TRUE, TRUE)
  expect_equal(X != X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor less-than-or-equals comparison works for expected inputs", {
  subs_expected <- allsubs(X)
  vals_expected <- c(F,T,T,T,T,T,T,T)
  expect_equal(X <= X2, sptensor(subs_expected, vals_expected, dims))
  expect_warning(X <= X2)
})

test_that("sparse tensor greater-than-or-equals comparison works for expected inputs", {
  subs_expected <- allsubs(X)
  vals_expected <- c(T,T,T,F,F,T,T,T)
  expect_equal(X >= X2, sptensor(subs_expected, vals_expected, dims))
  expect_warning(X >= X2)
})


test_that("sparse tensor & operator works for expected inputs", {
  subs_expected <- matrix(c(1,1,2), nrow = length(dims))
  vals_expected <- TRUE
  expect_equal(X & X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor & operator works for expected inputs", {
  subs_expected <- matrix(c(1,1,1, 2,2,1, 1,1,2), nrow = length(dims))
  vals_expected <- c(TRUE, TRUE, TRUE)
  expect_equal(X | X2, sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor Math works for expected inputs", {
  expect_equal(abs(-X), X)
  expect_equal(sqrt(X^2), X)

  subs_expected <- subs
  vals_expected <- round(vals)
  expect_equal(round(X), sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor Math works for inputs that cause tensor to become dense", {
  subs_expected <- allsubs(X)
  vals_expected <- exp(-c(10.6,0,0,0,20,0,0,0))
  expect_equal(exp(-X), sptensor(subs_expected, vals_expected, dims))
})

test_that("sparse tensor Summary works for expected inputs", {
  expect_equal(max(X), 20)
  expect_equal(sum(X), 30.6)
})

test_that("sparse tensor Complex works for expected inputs", {
  subs_expected <- subs
  real <- c(10, 20)
  imaginary <- c(30, 40)

  Z <- sptensor(subs, complex(real = real, imaginary = imaginary), dims)
  expect_equal(Re(Z), sptensor(subs_expected, real, dims))
  expect_equal(Im(Z), sptensor(subs_expected, imaginary, dims))
})
