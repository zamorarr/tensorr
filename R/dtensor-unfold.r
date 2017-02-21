#' @rdname unfolded_dtensor
#' @aliases unfolded_dtensor,Matrix,numeric,numeric-method
#' @export
#' @importClassesFrom Matrix Matrix
#' @importFrom assertive.base assert_are_identical
setMethod("unfolded_dtensor", c("Matrix", "numeric", "numeric"), function(mat, mode, tensor_dims) {
  # coerce doubles to int
  tensor_dims <- as.integer(tensor_dims)
  mode <- as.integer(mode)

  # check dimensions
  len_tensor <- as.integer(prod(tensor_dims))
  assert_are_identical(length(mat), len_tensor)
  assert_are_identical(nrow(mat), tensor_dims[mode])

  methods::new("unfolded_dtensor",
               mat = mat, mode = mode, tensor_dims = tensor_dims)
})


#' @rdname unfold
#' @aliases unfold,dtensor,numeric-method
#' @export
setMethod("unfold", signature(x = "dtensor", mode = "numeric"), function(x, mode) {
  subs <- allsubs(x)
  vals <- as.vector(x)
  dims <-  dim(x)

  newindices <- unfold_indices(mode, subs, vals, dims)
  i <- newindices$i
  j <- newindices$j
  newdims <- newindices$newdims

  indices <- rbind(i,j)
  indices <- vec_index(indices, newdims)
  ord <- order(indices)

  #mat <- matrix(vals[ord], nrow = newdims[1], ncol = newdims[2])
  mat <- Matrix::Matrix(vals[ord], nrow = newdims[1], ncol = newdims[2], sparse = FALSE)
  unfolded_dtensor(mat, mode, dims)
})

#' @rdname refold
#' @aliases refold,unfolded_dtensor-method
#' @export
setMethod("refold", "unfolded_dtensor", function(x) {
  mat <- x@mat
  vals <- mat@x
  dims <- x@tensor_dims

  arr <- array_index(seq_along(mat), dim(mat))
  i <- arr[1,]
  j <- arr[2,]

  subs <- refold_indices(i, j, x@mode, dims)
  indices <- vec_index(subs, dims)
  ord <- order(indices)
  dtensor(array(vals[ord], dims))
})

#' @rdname show
setMethod("show", "unfolded_dtensor", function(object) {
  x <- object

  # header message
  msg_dims <- paste(x@tensor_dims, "x", sep = "", collapse = "")
  msg_dims <- strtrim(msg_dims, nchar(msg_dims)-1)
  msg <- paste("<A", msg_dims,"unfolded sparse tensor along mode", x@mode,">", sep = " " )
  cat(msg)
  cat("\n")

  print(x@mat)
})
