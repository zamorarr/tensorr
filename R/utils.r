#' convert args i,j,... to array of coords.
#'
#' substitute missing args k with 1:dim[k]
#' @param dims tensor dimensions
#' @param ... indices
#' @importFrom assertive.base assert_are_identical
build_indices <- function(dims, ...) {
  # check that there as many indices as tensor dimensions
  nindices <- nargs() - 1L
  assert_are_identical(nindices, length(dims))

  # save calling environment
  penv <- parent.frame()

  # get indices
  indices <- as.list(match.call(expand.dots = FALSE)$`...`)

  # make new indices by filling out the missing ones
  newindices <- map2(indices, dims, fill_missing_indices, penv)

  # return grid as a matrix
  expand_indices(newindices)
}

# if an index is missing, return a range from 1:dim
fill_missing_indices <- function(index, dim, penv) {
  if (is.symbol(index)) {
    if (it_exists(index, penv)) eval(index, penv) # provided i or j value (by name)
    else seq_len(dim) # missing ... value
  }
  else if (is.null(index)) seq_len(dim) # missing i or j value
  else index # provided ... value (by value)
}

# does variable exist in environment?
it_exists <- function(var, env) {
  exists(as.character(var), env)
}

expand_indices <- function(...) {
  res <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  res <- as.matrix(res)
  res <- t(res)
  rownames(res) <- NULL
  apply(res, c(1,2), as.integer)
}

#' Vector Index
#'
#' Coverts a vector, matrix, or list of array indices to
#' linear indices.
#'
#' @param x vector, matrix, or list of numeric indices
#' @param dims dimensions
vec_index <- function(x, dims) {
  if(!any(is_numeric(x), is_list(x))) {
    stop("x must be a numeric or list of numerics", call. = FALSE)
  }

  # calculate the cumulative size of the tensor
  n <- length(dims)
  cumdims <- cumprod(c(1, dims[-n]))

  # calculate vector indices
  if (is_list(x)) map_int(x, vec_index_one, cumdims)
  else if (is.matrix(x)) as.vector(col_apply(x, vec_index_one, cumdims))
  else vec_index_one(x, cumdims)
}

#' @rdname vec_index
#' @param cumdims cumulative size of tensor
#' @importFrom assertive.types assert_is_numeric
vec_index_one <- function(x, cumdims) {
  assert_is_numeric(x)
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
row_apply <- function(x, f, ...) {
  res <- apply(x, 1, f, ...)

  if(!is.matrix(res)) matrix(res, nrow = nrow(x))
  else t(res)
}

#' convert matrix of indices to list of indices
#' @param m matrix
mat_to_listidx <- function(m) {
  seq_cols <- seq_len(ncol(m))
  map(seq_cols, function(col) unname(m[,col]))
}

#' convert list of indices to matrix of indcies
#' @param x list
list_to_matidx <- function(x) {
  d <- length(x[[1]])
  #vapply(x, function(i) as.integer(i), FUN.VALUE = integer(d))
  matrix(unlist(x), nrow = d)
}
