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
#' @export
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
#' @export
setGeneric("dtensor", function(x) standardGeneric("dtensor"))


#' Subscripts of non-zero values in a tensor
#'
#' @param x tensor
setGeneric("nzsubs", function(x) standardGeneric("nzsubs"))

#' Non-zero values in a tensor
#'
#' @param x tensor
setGeneric("nzvals", function(x) standardGeneric("nzvals"))
