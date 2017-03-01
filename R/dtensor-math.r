#' @rdname ttm
#' @aliases dtensor,Matrix,numeric,numeric-method
#' @export
#' @importClassesFrom Matrix Matrix
#' @importFrom assertive.base assert_are_identical
setMethod("ttm", c("dtensor", "Matrix", "numeric"), function(x, u, mode) {
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
  Yunfold <- unfolded_dtensor(Ymat, mode, Ydims)

  refold(Yunfold)
})

#' @rdname ttm
#' @aliases dtensor,matrix,numeric,numeric-method
#' @export
#' @importFrom assertive.base assert_are_identical
setMethod("ttm", c("dtensor", "matrix", "numeric"), function(x, u, mode) {
  U <- Matrix::Matrix(u)
  ttm(x, U, mode)
})

#' @rdname ttv
#' @aliases dtensor,numeric,numeric,numeric-method
#' @export
#' @importFrom assertive.properties assert_is_vector
setMethod("ttv", c("dtensor", "numeric", "numeric"), function(x, v, mode) {
  assert_is_vector(v)
  u <- Matrix::Matrix(v, nrow = 1L)
  Y <- ttm(x, u, mode)

  # remove the mode dimension since it is of size 1 now
  dtensor(drop(Y@x))
})
