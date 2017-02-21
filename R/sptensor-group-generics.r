warn_not_sparse <- "Operation converts zero values to non-zero values. Tensor is likely dense now"

#' Ops Methods for dense tensors
#'
#' @param e1,e2 at least one of them a sparse tensor
#' @name sptensor-Ops
#' @aliases Ops,sptensor,sptensor-method
#' @importFrom assertive.base assert_are_identical
#' @keywords internal
setMethod("Ops", c("sptensor", "sptensor"),  function(e1, e2) {
  # ensure tensor dimensions are equal
  dim1 <- dim(e1)
  dim2 <- dim(e2)
  assert_are_identical(dim1, dim2)

  # combine non-zero subscripts
  subs <- union_subs(nzsubs(e1), nzsubs(e2))

  # call op on the combined non-zero values
  vals <- methods::callGeneric(e1[subs], e2[subs])

  # call op on combined zero values
  val0 <- methods::callGeneric(0,0)
  if (val0 != 0) {
    warning(warn_not_sparse)
    subs0 <- intersect_subs(zsubs(e1), zsubs(e2))
    vals0 <- rep(val0, ncol(subs0))

    subs <- cbind(subs, subs0)
    vals <- c(vals, vals0)
  }

  # return new sparse tensor
  sptensor(subs, vals, dim1)

})

#' @rdname sptensor-Ops
setMethod("Ops", c("sptensor", "missing"), function(e1, e2) {
  # -X == 0 - X
  methods::callGeneric(0, e1)
})

#' @rdname sptensor-Ops
setMethod("Ops", c("sptensor","ANY"), function(e1, e2) {
  # only for scalars
  stopifnot(length(e2) == 1L)

  subs <- nzsubs(e1)
  vals <- nzvals(e1)
  dims <- dim(e1)

  # call op on non-zero values
  newvals <- methods::callGeneric(vals, e2)

  # call op on zero values
  val0 <- methods::callGeneric(0, e2)
  if (val0 != 0) {
    warning(warn_not_sparse)
    subs0 <- zsubs(e1)
    vals0 <- rep(val0, ncol(subs0))

    subs <- cbind(subs, subs0)
    newvals <- c(newvals, vals0)
  }

  sptensor(subs, newvals, dims)
})

#' @rdname sptensor-Ops
setMethod("Ops", c("ANY", "sptensor"), function(e1, e2) {
  # only for scalars
  stopifnot(length(e1) == 1L)

  subs <- nzsubs(e2)
  vals <- nzvals(e2)
  dims <- dim(e2)

  # call op on non-zero values
  newvals <- methods::callGeneric(e1, vals)

  # call op on zero values
  val0 <- methods::callGeneric(e1, 0)
  if (val0 != 0) {
    warning(warn_not_sparse)
    subs0 <- zsubs(e2)
    vals0 <- rep(val0, ncol(subs0))

    subs <- cbind(subs, subs0)
    newvals <- c(newvals, vals0)
  }

  sptensor(subs, newvals, dims)
})

#' Math Methods for sparse tensors
#'
#' @param x sparse tensor
#' @name sptensor-Math
#' @aliases Math,sptensor-method
#' @keywords internal
setMethod("Math", "sptensor", function(x) {
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <- dim(x)

  # call op on non-zero values
  newvals <- methods::callGeneric(vals)

  # call op on zero values
  val0 <- methods::callGeneric(0)
  if (val0 != 0) {
    warning(warn_not_sparse)
    subs0 <- zsubs(x)
    vals0 <- rep(val0, ncol(subs0))

    subs <- cbind(subs, subs0)
    newvals <- c(newvals, vals0)
  }

  sptensor(subs, newvals, dims)
} )

#' @rdname sptensor-Math
setMethod("cummax", "sptensor", function(x) stop("not implemented yet", call. = FALSE))

#' @rdname sptensor-Math
setMethod("cummin", "sptensor", function(x) stop("not implemented yet", call. = FALSE))

#' @rdname sptensor-Math
setMethod("cumprod", "sptensor", function(x) stop("not implemented yet", call. = FALSE))

#' @rdname sptensor-Math
setMethod("cumsum", "sptensor", function(x) stop("not implemented yet", call. = FALSE))


#' Math2 Methods for sparse tensors
#'
#' @param x sparse tensor
#' @param digits number of digits to be used in round or signif.
#' @name sptensor-Math2
#' @aliases Math2,sptensor-method
#' @keywords internal
setMethod("Math2", "sptensor", function(x, digits) {
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <- dim(x)

  # call math2 on the non-zero subscripts
  newvals <- methods::callGeneric(nzvals(x), digits)

  # return new sparse tensor
  sptensor(subs, newvals, dims)

})

#' Summary Methods for sparse tensors
#'
#' @param x sparse tensor
#' @param ... further arguments passed to or from methods.
#' @param na.rm logical: should missing values be removed?
#' @name sptensor-Summary
#' @aliases Summary,sptensor-method
#' @keywords internal
setMethod("Summary", "sptensor", function(x, ..., na.rm) {

  if (!missing(...)) stop("multiple arguments not implemented yet", call. = FALSE)

  vals <- nzvals(x)
  # there is at least one 0.
  if (length(nzvals(x)) < length(x)) vals <- c(0, vals)

  methods::callGeneric(vals, na.rm = na.rm)
})


#' Complex Methods for sparse tensors
#'
#' @param z sparse tensor
#' @name sptensor-Complex
#' @aliases Complex,sptensor-method
#' @keywords internal
setMethod("Complex", "sptensor", function(z) {
  subs <- nzsubs(z)
  vals <- nzvals(z)
  dims <- dim(z)

  # call math2 on the non-zero subscripts
  newvals <- methods::callGeneric(vals)

  # return new sparse tensor
  sptensor(subs, newvals, dims)
})

