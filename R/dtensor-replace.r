#' Replace values from a dense tensor
#'
#' @param x dtensor object
#' @param i numeric index, vector, list, or matrix
#' @param j numeric index
#' @param ... additional numeric indices
#' @param value replacement value(s)
#' @name dtensor-replace
NULL

#' @rdname dtensor-replace
#' @export
#' @aliases [<-,dtensor,ANY,ANY-method
setMethod("[<-",
  signature(x = "dtensor", i = "ANY", j = "ANY", value = "ANY"),
  function(x, i, j, ..., value) { # x[i=,j,...]
    x@x[i,j,...] <- value
    x
  }
)

#' @rdname dtensor-replace
#' @export
#' @aliases [<-,dtensor,ANY,missing-method
setMethod("[<-",
  signature(x = "dtensor", i = "ANY", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) { # x[i=,j,...]
    if ((nargs() == 2 & missing(value)) | (nargs() == 3 & !missing(value))) { # x[i]
      x@x[i] <- value
      x
    }
    else { # x[i, j = , ...]
      x@x[i,j,...] <- value
      x
    }
  }
)

#' @rdname dtensor-replace
#' @export
#' @aliases [<-,dtensor,list,missing-method
#' @importFrom assertive.properties assert_are_same_length
setMethod("[<-",
  signature(x = "dtensor", i = "list", j = "missing", value = "ANY"),
  function(x,i,j,..., value) {
    mat <- list_to_matidx(i)

    # dimensions should match
    dims <- dim(x)
    walk(i, ~assert_are_same_length(.x, dims))
    assert_are_same_length(i, value)

    # replace values
    x[mat] <- value
    x
  }
)

#' @rdname dtensor-replace
#' @export
#' @aliases [<-,dtensor,matrix,missing-method
#' @importFrom assertive.base assert_are_identical
setMethod("[<-",
  signature(x = "dtensor", i = "matrix", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    # dimensions should match
    dims <- dim(x)
    assert_are_identical(nrow(i), length(dims))
    assert_are_identical(ncol(i), length(value))

    # compare each col to the subscripts
    args <- mat_to_listidx(i)

    # inefficient because has to call for each replacement val?
    for (a in seq_along(args)) {
      x@x <- do.call(`[<-`, c(list(x@x), args[[a]], value = value[a]))
    }

    x
  }
)

