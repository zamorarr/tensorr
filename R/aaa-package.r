#' tensorr: sparse tensors in R
#'
#' tensorr provides methods to manipulate and store sparse tensors. Tensors are
#' multi-dimensional generalizations of matrices (two dimensional) and vectors
#' (one dimensional).
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Provide an efficient format to store sparse tensors in R.
#' \item Provide standard tensor operations such as multiplication and unfolding.
#' \item Provide standard tensor decomposition techniques such as CP and Tucker.
#' }
#'
#' @references
#' Many of the dense and sparse implementation ideas were adapted from
#' \itemize{
#' \item \cite{B. W. Bader and T. G. Kolda. Algorithm 862: MATLAB tensor classes
  #'  for fast algorithm prototyping, ACM Transactions on Mathematical Software
  #'  32(4):635-653, December 2006. }
#'  \item \cite{B. W. Bader and T. G. Kolda. Efficient MATLAB computations with
  #'  sparse and factored tensors, SIAM Journal on Scientific Computing
  #'  30(1):205-231, December 2007. }
#' }
#'
#' For a review on tensors, see
#' \itemize{
#' \item \cite{T. G. Kolda and B. W. Bader, Tensor Decompositions and
#' Applications, SIAM Review 51(3):455-500, September 2009}
#' }
#
#' @docType package
#' @name tensorr
#' @import purrr
NULL
