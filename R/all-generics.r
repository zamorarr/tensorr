#' Construct a sparse tensor
#'
#' Construct an \linkS4class{sptensor} from a matrix of subscripts for non-zero values,
#' a vector of non-zero values, and the numeric dimensions of the tensor.
#'
#' @param subs matrix with length(dims) rows and length(vals) cols. Each row
#' in the matrix corresponds to a different tensor dimension. Each column in the
#'  matrix represents a vector of subscripts pointing to a non-zero value in the
#'   tensor.
#' @param vals values of non-zero entries. The subscripts for the first value
#' are the first column of the \code{subs} matrix, the subscripts for the second value
#' are in the second column of the \code{subs} matrix, etc...
#' @param dims sizes of each dimension
#'
#' @examples
#' # A 2x2x2 sparse tensor
#' subs <- matrix(c(1,1,1, 1,1,2), c(3,2))
#' vals <- c(10,20)
#' dims <- c(2,2,2)
#' X <- sptensor(subs, vals, dims)
#'
#' @seealso \code{\link{sptensor-class}} for class documentation.
setGeneric("sptensor", function(subs, vals, dims) standardGeneric("sptensor"))

#' Construct a dense tensor
#'
#' Construct a \linkS4class{dtensor} from a multi-dimensional array.
#'
#' @param x n-dimensional R array
#' @examples
#' # A 2x2x2 dense tensor
#' arr <- array(data = c(1,0,0,0,1,0,0,0) , dim = c(2,2,2))
#' X <- dtensor(arr)
#'
#' @seealso \code{\link{dtensor-class}} for class documentation.
setGeneric("dtensor", function(x) standardGeneric("dtensor"))


#' Subscripts of non-zero values in a tensor
#'
#' @param x tensor
setGeneric("nzsubs", function(x) standardGeneric("nzsubs"))

#' Subscripts of zero values in a tensor
#'
#' @param x tensor
#' @keywords internal
setGeneric("zsubs", function(x) standardGeneric("zsubs"))

#' Subscripts of all values in a tensor
#'
#' @param x tensor
#' @keywords internal
setGeneric("allsubs", function(x) standardGeneric("allsubs"))

#' Non-zero values in a tensor
#'
#' @param x tensor
setGeneric("nzvals", function(x) standardGeneric("nzvals"))

#' Convert objects to sparse tensors
#'
#' @param x object
#' @param ... extra params
setGeneric("as_sptensor", function(x, ...) standardGeneric("as_sptensor"))

#' Convert objects to dense tensors
#'
#' @param x object
#' @param ... extra params
setGeneric("as_dtensor", function(x, ...) standardGeneric("as_dtensor"))

#' Construct an unfolded sparse tensor
#'
#' Construct an \linkS4class{unfolded_sptensor} from a sparse matrix,
#' mode, and dimensions of original tensor.
#'
#' @param mat sparse matrix representing unfolded tensor
#' @param mode dimension to unfold tensor along
#' @param tensor_dims original dimensions of tensor (useful for re-folding)
#'
#' @seealso \code{\link{unfolded_sptensor-class}} for class documentation.
setGeneric("unfolded_sptensor", function(mat, mode, tensor_dims) standardGeneric("unfolded_sptensor"))

#' Construct an unfolded dense tensor
#'
#' Construct an \linkS4class{unfolded_dtensor} from a dense matrix,
#' mode, and dimensions of original tensor.
#'
#' @param mat dense matrix representing unfolded tensor
#' @param mode dimension to unfold tensor along
#' @param tensor_dims original dimensions of tensor (useful for re-folding)
#'
#' @seealso \code{\link{unfolded_dtensor-class}} for class documentation.
setGeneric("unfolded_dtensor", function(mat, mode, tensor_dims) standardGeneric("unfolded_dtensor"))


#' Unfold (matricize) a tensor along a mode
#' @param x tensor
#' @param mode dimension to unfold along
setGeneric("unfold", function(x, mode) standardGeneric("unfold"))

#' Refold an unfolded tensor
#' @param x an unfolded tensor
setGeneric("refold", function(x) standardGeneric("refold"))

#' Tensor times matrix
#'
#' Calculates the n-mode product of a tensor and a matrix. Given a tensor X with
#' dimensions \eqn{I_1, I_2, ..., I_n, I_n+1, ... I_N} and a matrix U with
#' dimensions \eqn{J, I_n}, the resulting tensor after multiplication will have
#' dimension \eqn{I_1, I_2, ..., J, I_n+1, ... I_N}.
#'
#' @param x tensor
#' @param u matrix
#' @param mode mode along tensor to perform multiplication
#'
#' @references \cite{T. G. Kolda and B. W. Bader, Tensor Decompositions and
  #' Applications, SIAM Review 51(3):455-500, September 2009}
setGeneric("ttm", function(x, u, mode) standardGeneric("ttm"))

#' Tensor times vector
#'
#' Calculates the n-mode product of a tensor and a vector Given a tensor X with
#' dimensions \eqn{I_1, I_2, ..., I_n, I_n+1, ... I_N} and a vector v with
#' dimensions \eqn{J, 1}, the resulting tensor after multiplication will have
#' dimension \eqn{I_1, I_2, ..., I_n-1, I_n+1, ... I_N}. Note that the dimension
#' corresponding to the \code{mode} has been dropped.
#'
#' @param x tensor
#' @param v vector
#' @param mode mode along tensor to perform multiplication
#'
#' @references \cite{T. G. Kolda and B. W. Bader, Tensor Decompositions and
#' Applications, SIAM Review 51(3):455-500, September 2009}
setGeneric("ttv", function(x, v, mode) standardGeneric("ttv"))

#' Calculate the Frobenius norm of a tensor
#' @param x tensor
setGeneric("norm", function(x) standardGeneric("norm"))

#' Calculate the inner product of a pair of tensors
#' @param x,y tensors
setGeneric("innerprod", function(x,y) standardGeneric("innerprod"))

#' Calculate the outer product of a pair of tensors
#' @param x,y tensors
setGeneric("outerprod", function(x,y) standardGeneric("outerprod"))

#' @rdname outerprod
setGeneric("ttt", function(x, y) standardGeneric("ttt"))
