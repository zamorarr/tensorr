#' Virtual S4 class for all tensor objects
#'
#' The \code{tensor} class is a base virtual tensor class for objects in this
#' repository.
#' @keywords internal
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
setClass("sptensor",
         slots = list(subs = "matrix", vals = "ANY", dims = "integer"),
         contains = "tensor")

#' An S4 class for a dense tensor
#'
#' Simple wrapper around an n-dimensional array.
#'
#' @slot x n dimensional array
setClass("dtensor", slots = list(x = "array"), contains = "tensor")

#' Virtual S4 class for unfolded tensor objects
#'
#' The \code{unfolded_tensor} class is a base virtual class for unfolded tensors in this
#' repository.
#' @keywords internal
setClass("unfolded_tensor", contains = "VIRTUAL")

#' An S4 class for an unfolded sparse tensor
#'
#' An unfolded ("matriczied") tensor along a specified dimension. Can be easily
#' refolded back into a tensor with command \code{refold}
#'
#' @slot mat sparse matrix representing unfolded tensor
#' @slot mode dimension along which tensor was unfolded
#' @slot tensor_dims dimensions of original tensor
#'
#' @importClassesFrom Matrix sparseMatrix
setClass("unfolded_sptensor",
         slots = list(mat = "sparseMatrix", mode = "integer", tensor_dims = "integer"),
         contains = "unfolded_tensor")

#' An S4 class for an unfolded dense tensor
#'
#' An unfolded ("matriczied") tensor along a specified dimension. Can be easily
#' refolded back into a tensor with command \code{refold}
#'
#' @slot mat sparse matrix representing unfolded tensor
#' @slot mode dimension along which tensor was unfolded
#' @slot tensor_dims dimensions of original tensor
#'
#' @importClassesFrom Matrix Matrix
setClass("unfolded_dtensor",
         slots = list(mat = "Matrix", mode = "integer", tensor_dims = "integer"),
         contains = "unfolded_tensor")
