#' @rdname  as_dtensor
#' @aliases as_dtensor,sptensor-method
#' @export
setMethod("as_dtensor", "sptensor",
  function(x) {
    subs <- nzsubs(x)
    vals <- nzvals(x)
    dims <- dim(x)

    # initialize empty data array
    # logical, integer, numeric/double, complex, character
    emptyval <-
      if (all(is.logical(vals))) FALSE
      else if (all(is.integer(vals))) 0L
      else if (all(is.character(vals))) ""
      else 0

    data <- array(emptyval, dim = dims)

    # insert non-zero values
    index <- vec_index(subs, dims)
    data[index] <- vals

    # return dense tensor
    res <- dtensor(data)

    # add dimnames if they exist
    dnames <- dimnames(x)
    if (is.null(dnames)) {
      dimnames(res) <- vector("list", length(dims))
    } else {
      dimnames(res) <- dnames
    }

    res
  })

#' @rdname as_dtensor
#' @aliases as_dtensor,array-method
#' @export
setMethod("as_dtensor", "array", function(x) dtensor(x) )

#' @rdname as.vector
#' @aliases as.vector,dtensor-method
#' @export
#' @keywords internal
setMethod("as.vector", "dtensor", function(x) as.vector(x@x))
