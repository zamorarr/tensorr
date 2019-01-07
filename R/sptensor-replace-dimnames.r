#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,numeric,missing-method
setMethod("[<-",
  signature(x = "sptensor", i = "character", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {

    if (nargs() == 3 & !missing(value)) { # x[i]
      stop("cannot index tensor with single vector of type character", call. = FALSE)
    }
    else { # x[i, j = , ...]
      mat <- build_indices(x, i = i, j = NULL, ...)$oldsubs
    }

    replace_sptensor(x, mat, value)
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,missing,numeric-method
setMethod("[<-",
  signature(x = "sptensor", i = "missing", j = "character", value = "ANY"),
  function(x, i, j, ..., value) { # x[i=,j,...]
    mat <- build_indices(x, i = NULL, j = j, ...)$oldsubs
    replace_sptensor(x, mat, value)
  }
)

#' @rdname sptensor-replace
#' @export
#' @aliases [<-,sptensor,numeric,numeric-method
setMethod("[<-",
  signature(x = "sptensor", i = "character", j = "character", value = "ANY"),
  function(x, i, j, ..., value) { # x[i,j,...]
    mat <- build_indices(x, i, j, ...)$oldsubs
    replace_sptensor(x, mat, value)
  }
)
