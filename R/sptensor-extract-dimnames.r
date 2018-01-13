#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,numeric,missing-method
setMethod("[",
          signature(x = "sptensor", i = "character", j = "missing", drop = "ANY"),
          function(x, i, j, ..., drop = FALSE) {

            if ((nargs() == 2 & missing(drop)) | (nargs() == 3 & !missing(drop))) { # x[i]
              stop("cannot index tensor with single vector of type character", call. = FALSE)
            }
            else { # x[i, j = , ...]
              extract_sptensor(x, i = i, j = NULL, ..., drop = drop)
            }
          }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,missing,numeric-method
setMethod("[",
          signature(x = "sptensor", i = "missing", j = "character", drop = "ANY"),
          function(x, i, j, ..., drop = FALSE) { # x[i=,j,...]
            extract_sptensor(x, i = NULL, j = j, ..., drop = drop)
          }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,numeric,numeric-method
setMethod("[",
          signature(x = "sptensor", i = "character", j = "character", drop = "ANY"),
          function(x, i, j, ..., drop = FALSE) { # x[i,j,...]
            extract_sptensor(x, i, j, ..., drop = drop)
          }
)
