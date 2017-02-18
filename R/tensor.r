#' Test if the object is a tensor
#' @param x object
#' @export
is_tensor <- function(x) inherits(x, "tensor")

#' Length of a tensor
#'
#' The total number of values in a tensor. Equal to the product of
#' the dimensions.
#'
#' @param x tensor
#' @export
setMethod("length", "tensor", function(x) prod(dim(x)) )

#' Dimensions of a tensor
#'
#' @name dim
#' @param x tensor
NULL

#' Vectorize values of a tensor
#'
#' @name as.vector
#' @param x tensor
NULL

#' Show a tensor
#'
#' @name show
#' @param object tensor
NULL
