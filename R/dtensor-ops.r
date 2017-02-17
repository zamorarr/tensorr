#' Ops Methods for dense tensors
#'
#' @param e1,e2 at least one of them a dense tensor
#' @name dtensor-Ops
#' @aliases Ops,dtensor,dtensor-method
setMethod("Ops", c("dtensor","dtensor"), function(e1, e2) dtensor(methods::callGeneric(e1@x, e2@x)) )

#' @rdname dtensor-Ops
setMethod("Ops", c("dtensor","sptensor"), function(e1, e2) methods::callGeneric(e1, as_dtensor(e2)) )

#' @rdname dtensor-Ops
setMethod("Ops", c("sptensor","dtensor"), function(e1, e2) methods::callGeneric(as_dtensor(e1), e2) )

#' @rdname dtensor-Ops
setMethod("Ops", c("dtensor", "missing"), function(e1, e2) dtensor(methods::callGeneric(0, e1@x)) )

#' @rdname dtensor-Ops
setMethod("Ops", c("dtensor","ANY"), function(e1, e2) dtensor(methods::callGeneric(e1@x, e2)) )

#' @rdname dtensor-Ops
setMethod("Ops", c("ANY", "dtensor"), function(e1, e2) dtensor(methods::callGeneric(e1, e2@x)) )

#' Math Methods for dense tensors
#'
#' @param x dense tensor
#' @name dtensor-Math
#' @aliases Math,dtensor-method
setMethod("Math", "dtensor", function(x) dtensor(methods::callGeneric(x@x)) )

#' Math2 Methods for dense tensors
#'
#' @param x dense tensor
#' @param digits number of digits to be used in round or signif.
#' @name dtensor-Math2
#' @aliases Math2,dtensor-method
setMethod("Math2", "dtensor", function(x, digits) dtensor(methods::callGeneric(x@x, digits)) )

#' Summary Methods for dense tensors
#'
#' @param x dense tensor
#' @param ... further arguments passed to or from methods.
#' @param na.rm logical: should missing values be removed?
#' @name dtensor-Summary
#' @aliases Summary,dtensor-method
setMethod("Summary", "dtensor", function(x, ..., na.rm = FALSE) methods::callGeneric(x@x, na.rm))

#' Complex Methods for dense tensors
#'
#' @param z dense tensor
#' @name dtensor-Complex
#' @aliases Complex,dtensor-method
setMethod("Complex", "dtensor", function(z) dtensor(methods::callGeneric(z@x)) )
