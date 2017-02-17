#' Replace values from a sparse tensor
#'
#' @param x sptensor object
#' @param i numeric index, vector, list, or matrix
#' @param j numeric index
#' @param ... additional numeric indices
#' @param value replacement value(s)
#' @name sptensor-replace
NULL

# empty subscripts
# x[]
#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,missing,missing-method
setMethod("[<-",
  signature(x = "sptensor", i = "missing", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    if (missing(...)) { # x[]
      stop("Not Implemented", call. = FALSE)
    } else { # x[i=,j=,...]
      mat <- build_indices(dim(x),i = NULL, j = NULL, ...)
      x[mat] <- value
      x
    }
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,numeric,missing-method
setMethod("[<-",
  signature(x = "sptensor", i = "numeric", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {

    if (nargs() == 3 & !missing(value)) { # x[i]
      mat <- array_index(i, dim(x))
      x[mat] <- value
      x
    }
    else { # x[i, j = , ...]
      mat <- build_indices(dim(x),i = i, j = NULL ,...)
      x[mat] <- value
      x
    }
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,missing,numeric-method
setMethod("[<-",
  signature(x = "sptensor", i = "missing", j = "numeric", value = "ANY"),
  function(x, i, j, ..., value) { # x[i=,j,...]
    mat <- build_indices(dim(x), i = NULL, j = j, ...)
    x[mat] <- value
    x
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,numeric,numeric-method
setMethod("[<-",
  signature(x = "sptensor", i = "numeric", j = "numeric", value = "ANY"),
  function(x, i, j, ..., value) { # x[i,j,...]
    mat <- build_indices(dim(x),i,j,...)
    x[mat] <- value
    x
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,list,missing-method
setMethod("[<-",
  signature(x = "sptensor", i = "list", j = "missing", value = "ANY"),
  function(x,i,j,..., value = FALSE) {
    d <- length(dim(x))
    mat <- vapply(i, function(.i) as.integer(.i), FUN.VALUE = integer(d))
    x[mat] <- value
    x
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,matrix,missing-method
#' @importFrom assertive.base assert_are_identical
setMethod("[<-",
  signature(x = "sptensor", i = "matrix", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    # nrows in index matrix should be number of dims
    dims <- dim(x)
    assert_are_identical(nrow(i), length(dims))

    # check if "type" arg in the ellipsis
    type <- dots_arg("type", "vector", ...)

    # replace values in sptensor
    replace_sptensor(x, i, value)
  }
)

#' Replace values in a sparse tensor
#'
#' Replaces values in sparse tensor and returns a new sparse tensor.
#'
#' @param x sptensor
#' @param idxmat matrix of indices
#' @param value replacement value(s)
replace_sptensor <- function(x, idxmat, value) {

  newvals <- x@vals
  dims <- dim(x)

  # non-zero matches
  matching <- col_apply(idxmat, matches, x)
  matching <- as.vector(matching)
  nonzero <- matching > 0 & !is.na(matching)

  # matching non-zero tensor subs/values
  newvals[matching[nonzero]] <- value[nonzero]

  # non-matching indices. add these in
  # throw an error for is.na(matching)?
  if (any(is.na(matching))) stop("index out of bounds", call. = FALSE)
  nomatch <- which(matching == 0)
  newsubs <- idxmat[, nomatch, drop = FALSE]
  newsubs <- cbind(x@subs, newsubs)
  newvals <- c(newvals, value[nomatch])

  sptensor(newsubs, newvals, dims)
}
