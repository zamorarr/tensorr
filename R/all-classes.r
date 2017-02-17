#' Virtual S4 class for all tensor objects
#'
#' The \code{tensor} class is a base virtual tensor class for objects in this
#' repository.
setClass("tensor", contains = "VIRTUAL")

#' An S4 class for a sparse tensor
#'
#' Stores the tensor in co-ordinate (COO) format. Non-zero entries are stored
#' by their subscripts (i1,i2,i3,...,in) the \code{subs} matrix and their vals
#' in the \code{vals} vector.
#'
#' @slot subs matrix with length(dims) rows and length(vals) cols.
#' @slot vals values of non-zero entries.
#' @slot dims sizes of each dimension
#'
#' @export
setClass("sptensor",
         slots = list(subs = "matrix", vals = "ANY", dims = "numeric"),
         contains = "tensor")

#' An S4 class for a dense tensor
#'
#' Simple wrapper around an n-dimensional array.
#'
#' @slot x n dimensional array
#' @export
setClass("dtensor", slots = list(x = "array"), contains = "tensor")
