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

#' @rdname outerprod
#' @aliases outerprod,sptensor,sptensor-method
#' @export
#' @importFrom assertive.properties assert_have_same_dims
setMethod("outerprod", signature(x = "sptensor", y = "sptensor"), function(x,y) {
  # dimensions must match
  assert_have_same_dims(x,y)

  # new dimensions
  newdims <- c(dim(x), dim(y))

  # new subscripts
  xsubs <- nzsubs(x)
  ysubs <- nzsubs(y)

  cols <- ncol(xsubs)
  seq_cols <- seq_len(cols)
  newsubs <- map(seq_cols, function(i) {
    col_i <- rep.int(xsubs[,i], cols)
    rbind(matrix(col_i, ncol = cols), ysubs)
  })

  newsubs <- reduce(newsubs, cbind)

  # new vals
  rows <- nrow(xsubs)
  seq_cols <- seq_len(ncol(newsubs))
  newvals <- map_dbl(seq_cols, function(i) {
    sub1 <- newsubs[1:rows,i,drop=FALSE]
    sub2 <- newsubs[(rows+1):(2*rows),i,drop=FALSE]
    x[sub1] * y[sub2]
  })

  # return new sptensor
  sptensor(newsubs, newvals, newdims)

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
