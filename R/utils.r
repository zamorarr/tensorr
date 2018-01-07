#' convert args i,j,... to array of coords.
#'
#' substitute missing args k with 1:dim[k]
#' @param x tensor
#' @param ... indices
#' @importFrom assertive.base assert_are_identical
#' @keywords internal
build_indices <- function(x,i,j, ...) {
  # check that there as many indices as tensor dimensions
  dims <- dim(x)
  nindices <- nargs() - 1L
  assert_are_identical(nindices, length(dims))

  # get indices
  extra_indices <- as.list(substitute(list(...)))[-1L]
  extra_indices <- lapply(extra_indices, function(v) if (is.name(v)) NULL else eval(v))
  indices <- c(list(i), list(j), extra_indices)

  # check if provided indices are strings (dimnames)
  are_chars <- vapply(indices, is.character, logical(1L))
  indices[are_chars] <- dimnames_to_indices(x, indices[are_chars], which(are_chars))

  # make new indices by filling out the missing ones
  filled_indices <- map2(indices, dims, fill_missing_indices)
  new_indices <- lapply(filled_indices, seq_along)

  # return grid as a matrix
  orig_indices <- expand_indices(filled_indices)
  new_indices <- expand_indices(new_indices)

  assertive.base::assert_are_identical(nrow(orig_indices), length(dims))
  assertive.base::assert_are_identical(nrow(new_indices), length(dims))

  list(oldsubs = orig_indices, newsubs = new_indices)
}

#' Fill NULL indices with a range from 1:dim
#'
#' @param index a numeric value or NULL
#' @param dim size of dimension
fill_missing_indices <- function(index, dim, ...) {
  stopifnot(is.null(index) || all(index > 0) || all(index < 0))

  if (is.null(index)) { # missing value - fill with all indices in that dimension
    seq_len(dim)
  } else if (all(index < 0)) { # negative values - exclude these indices
    setdiff(seq_len(dim), abs(index))
  } else { # positive values - include these indices
    index
  }

}

expand_indices <- function(...) {
  res <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  res <- as.matrix(res)
  res <- t(res)
  rownames(res) <- NULL
  dimnames(res) <- NULL
  apply(res, c(1,2), as.integer)
}

#' Vector Index
#'
#' Coverts a vector, matrix, or list of array indices to
#' linear indices.
#'
#' @param x vector, matrix, or list of numeric indices
#' @param dims dimensions
#' @keywords internal
vec_index <- function(x, dims) {
  # check that all inputs are numeric
  assert_all_are_numeric(x)

  # calculate the cumulative size of the tensor
  n <- length(dims)
  cumdims <- cumprod(c(1, dims[-n]))

  # calculate vector indices
  if (is.list(x)) map_int(x, vec_index_one, cumdims)
  else if (is.matrix(x)) as.vector(col_apply(x, vec_index_one, cumdims))
  else vec_index_one(x, cumdims)
}

#' @rdname vec_index
#' @param cumdims cumulative size of tensor
#' @importFrom assertive.types assert_is_numeric
#' @keywords internal
vec_index_one <- function(x, cumdims) {
  as.integer(sum( (x - 1) * cumdims) + 1)
}

#' Array Index
#'
#' Convert a vector of linear indices to a a matrix
#'  of array indices. Each linear index will map to
#'  a column in the matrix.
#'
#' @param x vector of linear indices
#' @param dims dimensions
#'
#' @importFrom assertive.types assert_is_numeric
#' @keywords internal
array_index <- function(x, dims) {
  # check if linear indices are numbers
  assert_is_numeric(x)

  # calculate the cumulative size of the tensor
  n <- length(dims)
  cumdims <- cumprod(c(1, dims[-n]))

  # allocate space for the result
  res <- matrix(0, nrow = n, ncol = length(x))

  # calculate the array indices
  for (i in rev(seq_len(n))) {
    cumdim <- cumdims[i]
    res[i,] <- as.integer(ceiling(x / cumdim))
    rem <- x %% cumdim
    x <- ifelse(rem == 0, cumdim, rem)
  }

  # return result
  res
}

#' Apply a function to columns of a matrix
#'
#' Convenience wrapper around apply. Always returns a matrix.
#' @param x matrix
#' @param f function to apply to each column
#' @param ... extra args to f
#' @keywords internal
col_apply <- function(x, f, ...) {
  res <- apply(x, 2, f, ...)

  if(!is.matrix(res)) matrix(res, ncol = ncol(x))
  else res
}

#' Apply a function to rows of a matrix
#'
#' Convenience wrapper around apply. Always returns a matrix.
#' @param x matrix
#' @param f function to apply to each row
#' @param ... extra args to f
#' @keywords internal
row_apply <- function(x, f, ...) {
  res <- apply(x, 1, f, ...)

  if(!is.matrix(res)) matrix(res, nrow = nrow(x))
  else t(res)
}

#' convert matrix of indices to list of indices
#' @param m matrix
#' @keywords internal
mat_to_listidx <- function(m) {
  seq_cols <- seq_len(ncol(m))
  map(seq_cols, function(col) unname(m[,col]))
}

#' convert list of indices to matrix of indcies
#' @param x list
#' @keywords internal
list_to_matidx <- function(x) {
  d <- length(x[[1]])
  #vapply(x, function(i) as.integer(i), FUN.VALUE = integer(d))
  matrix(unlist(x), nrow = d)
}

#' Convert dimnames to tensor indices
#' @param x tensor
#' @param dnames selected dimnames
#' @param dimensions selected dimensions
#' @keywords internal
dimnames_to_indices <- function(x, dnames, dimensions) {
  indices <- mapply(function(a,b) which(a == b), dnames, dimnames(x)[dimensions], SIMPLIFY = FALSE)
  L <- vapply(indices, length, integer(1L))
  if (any(L < 1)) {
    stop(paste0("dimname not found: ", dnames[L < 1]), call. = FALSE)
  }

  indices
}
