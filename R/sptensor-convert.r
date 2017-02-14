#' @rdname as_sptensor
#' @aliases as_sptensor,sptensor-method
#' @export
setMethod("as_sptensor", "sptensor", function(x) x)

# from dense tensor
#' @rdname as_sptensor
#' @aliases as_sptensor,dtensor-method
#' @export
setMethod("as_sptensor", "dtensor", function(x) {
  dims <- dim(x)
  vals <- x@x

  # non-zero values
  nonzero <- which(vals != 0)
  nonzero_subs <- array_index(nonzero, dims)
  nonzero_vals <- vals[nonzero]

  sptensor(nonzero_subs, nonzero_vals, dims)
})

# from data frame
#' @rdname as_sptensor
#' @aliases as_sptensor,data.frame-method
#' @param valcol column to use for the tensor values. all other columns are treated as incides
#' @param dims dimensions of tensor. If not provided, the maximum value for each of the indices is used.
#' @export
setMethod("as_sptensor", "data.frame", function(x, valcol = NULL, dims = NULL) {

  # if valcol not provided, use last column name
  if (is.null(valcol)) valcol <- colnames(x)[ncol(x)]

  # valcol index
  valcolidx <- which(colnames(x) == valcol)

  #  if dims not provided, use last column
  x_indices <- x[,-valcolidx]
  if (is.null(dims)) dims <- map_dbl(x_indices, max)

  # build subscript matrix
  subs <- t(as.matrix(x_indices))
  dimnames(subs) <- NULL

  # vals
  vals <- x[[valcol]]

  # build tensor
  sptensor(subs, vals, dims)
})

#' @rdname as.vector
#' @aliases as.vector,sptensor-method
#' @export
#setMethod("as.vector", c(x = "sptensor"), function(x) x[seq_along(x)])
