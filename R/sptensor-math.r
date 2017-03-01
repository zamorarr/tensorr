#' @rdname norm
#' @aliases norm,sptensor-method
#' @export
setMethod("norm", "sptensor", function(x) sqrt(sum(x@vals^2)))

#' @rdname innerprod
#' @aliases innerprod,sptensor,sptensor-method
#' @export
#' @importFrom assertive.properties assert_have_same_dims
setMethod("innerprod", signature(x = "sptensor", y = "sptensor"), function(x,y) {
  # dimensions must match
  assert_have_same_dims(x,y)

  xsubs <- nzsubs(x)
  ysubs <- nzsubs(y)

  subs <- union_subs(xsubs,ysubs)

  sum(x[subs] * y[subs])
})

#' @rdname ttm
#' @aliases sptensor,Matrix,numeric,numeric-method
#' @export
#' @importClassesFrom Matrix Matrix
#' @importFrom assertive.base assert_are_identical
setMethod("ttm", c("sptensor", "Matrix", "numeric"), function(x, u, mode) {
  # check dimensions are compatible
  Xdims <- dim(x)
  assert_are_identical(Xdims[mode], ncol(u))

  # create new dimensions
  Ydims <- Xdims
  Ydims[mode] <- nrow(u)

  # unfold X along mode
  Xunfold <- unfold(x, mode)
  Xmat <- Xunfold@mat

  # multiply by mat
  Ymat <- u %*% Xmat

  # refold tensor
  Yunfold <- unfolded_sptensor(Ymat, mode, Ydims)
  refold(Yunfold)
})

#' @rdname ttm
#' @aliases sptensor,matrix,numeric,numeric-method
#' @export
#' @importFrom assertive.base assert_are_identical
setMethod("ttm", c("sptensor", "matrix", "numeric"), function(x, u, mode) {
  U <- Matrix::Matrix(u)
  ttm(x, U, mode)
})

#' @rdname ttv
#' @aliases sptensor,numeric,numeric,numeric-method
#' @export
#' @importFrom assertive.properties assert_is_vector
setMethod("ttv", c("sptensor", "numeric", "numeric"), function(x, v, mode) {
  assert_is_vector(v) # and not a matrix
  u <- Matrix::Matrix(v, nrow = 1L)
  Y <- ttm(x, u, mode)

  # remove the mode dimension since it is of size 1 now
  squeeze(Y, mode)
})

#' @rdname ttv
#' @aliases sptensor,numeric,numeric,numeric-method
#' @export
#' @importClassesFrom Matrix sparseVector
#' @importFrom assertive.properties assert_is_vector
setMethod("ttv", c("sptensor", "sparseVector", "numeric"), function(x, v, mode) {
  u <- Matrix::Matrix(v, nrow = 1L, ncol = length(v))
  Y <- ttm(x, u, mode)

  # remove the mode dimension since it is of size 1 now
  squeeze(Y, mode)
})
