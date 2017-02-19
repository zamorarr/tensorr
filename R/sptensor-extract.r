#' Extract values from a sparse tensor
#'
#' There are multiple ways to provide indices for a sparse tensor. For example,
#' if you have a three-dimensional tensor you can provide indices separated
#' by a comma or a numeric vector of linear indices:
#' \itemize{
#' \item{c(x[1,1,1], x[1,1,2])}
#' \item{x[c(1, 5)]}
#' }
#' However for tensors with high dimensions this can be cumbersome to write.
#' Therefore the \code{tensor} class also allows you to extract values with
#' indices in matrix or list form, which are more suited for non-interactive
#' coding.
#' \itemize{
#' \item{x[matrix(c(1,1,1,1,1,2), nrow = 3)]}
#' \item{x[list(c(1,1,1), c(1,1,2))]}
#' }
#'
#' @param x sptensor object
#' @param i numeric index, vector, list, or matrix
#' @param j numeric index
#' @param ... additional numeric indices
#' @param drop whether to drop dimensions
#' @name sptensor-extract
NULL

# empty subscripts
# x[]
#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,missing,missing-method
setMethod("[",
  signature(x = "sptensor", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    if (missing(...)) { # x[]
      x
    } else { # x[i=,j=,...]
      mat <- build_indices(dim(x), i = NULL, j = NULL, ...)
      extract_sptensor(x, mat, drop)
    }
  }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,numeric,missing-method
setMethod("[",
  signature(x = "sptensor", i = "numeric", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {

    if ((nargs() == 2 & missing(drop)) | (nargs() == 3 & !missing(drop))) { # x[i]
      mat <- array_index(i, dim(x))
      extract_vec(x, mat)
    }
    else { # x[i, j = , ...]
      mat <- build_indices(dim(x),i = i, j = NULL ,...)
      extract_sptensor(x, mat, drop)
    }
  }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,missing,numeric-method
setMethod("[",
  signature(x = "sptensor", i = "missing", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) { # x[i=,j,...]
    mat <- build_indices(dim(x), i = NULL, j = j, ...)
    extract_sptensor(x, mat, drop)
  }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,numeric,numeric-method
setMethod("[",
  signature(x = "sptensor", i = "numeric", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) { # x[i,j,...]
    mat <- build_indices(dim(x),i,j,...)

    if (ncol(mat)  == 1) extract_vec(x, mat)
    else extract_sptensor(x, mat, drop)
  }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,list,missing-method
setMethod("[",
  signature(x = "sptensor", i = "list", j = "missing", drop = "ANY"),
  function(x,i,j,...,drop = FALSE) {
    mat <- list_to_matidx(i)
    extract_vec(x, mat)
  }
)

#' @rdname sptensor-extract
#' @export
#' @aliases [,sptensor,matrix,missing-method
setMethod("[",
  signature(x = "sptensor", i = "matrix", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    extract_vec(x, i)
  }
)

#' Extract indices from a sparse tensor
#'
#' Extracts indices from tensor and returns as a vector.
#'
#' @param x sptensor
#' @param idxmat matrix of indices
#'
extract_vec <- function(x, idxmat) {
  # check dimensions
  dims <- dim(x)
  assert_are_identical(nrow(idxmat), length(dims))

  # match indices
  matching <- col_apply(idxmat, matches, x)
  matching <- as.vector(matching)

  # non-zero values
  vals <- nzvals(x)

  # get matching value if exists
  map_dbl(matching, function(m) {
    if (!is.na(m) & m > 0) vals[m]
    else m # m is NA or 0
  })
}

#' Extract indices from a sparse tensor
#'
#' Extracts indices from a sparse tensor and returns as a new tensor.
#' Can also drop dimensions of the tensor that have size 1.
#'
#' @param x sptensor
#' @param idxmat matrix of indices requested
#' @param drop drop dimensions of size 1?
#'
#' @importFrom assertive.base assert_are_identical
extract_sptensor <- function(x, idxmat, drop = FALSE) {
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <- dim(x)

  # check dimensions
  assert_are_identical(nrow(idxmat), length(dims))

  # non-zero matches
  matching <- col_apply(idxmat, matches, x)
  matching <- as.vector(matching)
  nonzero_matches <- matching[matching > 0 & !is.na(matching)]

  # matching non-zero tensor subs/values
  matchsubs <- subs[, nonzero_matches, drop = FALSE]
  matchvals <- vals[nonzero_matches]

  # re-calculate dimensions
  newdims <- row_apply(idxmat, function(r) length(unique(r)))
  newdims <- as.vector(newdims)

  # check if vals empty - no matches found
  if (length(matchvals) == 0) {
    newsubs <- array(NA_integer_, dim = c(length(dims), 1L))
    newvals <- NA_real_
  } else {
    # re-number the subscripts
    newsubs <- row_apply(matchsubs, rank, ties.method = "min")
    newvals <- matchvals
  }

  res <- sptensor(newsubs, newvals, newdims)

  if (drop) squeeze(res)
  else res
}

#' Remove tensor dimensions of size 1
#'
#' @param x sptensor
squeeze <- function(x) {
  # tensor values
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <- dim(x)

  # keep dimensions that have size > 1
  keep <- dims > 1
  newdims <- dims[keep]
  newsubs <- subs[keep,,drop=FALSE]

  sptensor(newsubs, vals, newdims)
}

#' Match index in sparse tensor
#'
#' Matches a given index to the index of a non-zero value
#' in a sparse tensor.  Returns the index if there is match.
#' Returns 0 if there is no match. Returns NA if the index
#' is out of bounds.
#'
#' @param idx index
#' @param x sptensor
#' @importFrom assertive.base assert_all_are_not_na
matches <- function(idx, x) {
  subs <- nzsubs(x)
  dims <- dim(x)

  # check index does not have NAs in it
  assert_all_are_not_na(idx)
  if (any(idx > dims)) return(NA_integer_) # index out of bounds

  # ask if index matches any subscript
  idx <- as.integer(idx)
  match <- col_apply(subs, function(s) identical(s, idx))
  match <- as.vector(match)

  # get matches
  which_match <- which(match)
  len_res <- length(which_match)

  if (len_res == 0L) 0L # no match
  else if (len_res == 1L) which_match # one match found (good!)
  else stop("Multiple matches found. Tensor should not have duplicate subscripts", call. = FALSE)
}
