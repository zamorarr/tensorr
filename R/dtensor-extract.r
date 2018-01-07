#' Extract values from a dense tensor
#'
#' @param x dtensor object
#' @param i numeric index, vector, list, or matrix
#' @param j numeric index
#' @param ... additional numeric indices
#' @param drop whether to drop dimensions
#' @name dtensor-extract
NULL

# empty subscripts
# x[]
#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,missing,missing-method
setMethod("[",
  signature(x = "dtensor", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    if (missing(...)) x # x[]
    else { # x[i=,j=,...]
      dtensor(x@x[i=,j=,...,drop=drop])
    }
  }
)

#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,numeric,missing-method
setMethod("[",
  signature(x = "dtensor", i = "numeric", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    if ((nargs() == 2 & missing(drop)) | (nargs() == 3 & !missing(drop))) { # x[i]
      x@x[i]
    }
    else { # x[i, j = , ...]
      dtensor(x@x[i,j=, ..., drop = drop])
    }
  }
)

#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,missing,numeric-method
setMethod("[",
  signature(x = "dtensor", i = "missing", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) { # x[i=,j,...]
    dtensor(x@x[i=,j,...,drop=drop])
  }
)

#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,numeric,numeric-method
setMethod("[",
  signature(x = "dtensor", i = "numeric", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) { # x[i,j,...]
    mat <- build_indices(x,i,j,...)
    res <- x@x[i,j,...,drop=drop]
    if (length(res) > 1) dtensor(res)
    else as.vector(res)
  }
)

#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,matrix,missing-method
#' @importFrom assertive.base assert_are_identical
setMethod("[",
  signature(x = "dtensor", i = "matrix", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    # dimensions should match
    dims <- dim(x)
    assert_are_identical(nrow(i), length(dims))

    # compare each col to the subscripts
    idxlist <- mat_to_listidx(i)
    x[idxlist, drop = drop]
  }
)

#' @rdname dtensor-extract
#' @export
#' @aliases [,dtensor,list,missing-method
setMethod("[",
  signature(x = "dtensor", i = "list", j = "missing", drop = "ANY"),
  function(x,i,j,...,drop = FALSE) {
    map_dbl(i, function(a) do.call(`[`, c(list(x@x), a)))
  }
)


