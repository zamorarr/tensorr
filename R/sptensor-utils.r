#' Remove tensor dimensions of size 1
#'
#' @param x sptensor
#' @param todrop specific dimensions to drop. If NULL, will drop all dimensions
#' of size 1
squeeze <- function(x, todrop = NULL) {
  # tensor values
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <- dim(x)

  # keep dimensions that have size > 1
  if (is.null(todrop)) {
    todrop <- which(dims < 2)
  }

  newdims <- dims[-todrop]
  newsubs <- subs[-todrop,,drop=FALSE]

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
#' @keywords internal
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

#' Apply set function to subscript arrays
#'
#' @param subs1,subs2 subscript arrays
#' @param f set function to apply. Ex. union, setdiff, intersect
#' @keywords internal
setops_subs <- function(subs1, subs2, f) {
  # convert subscripts to list
  subs1list <- mat_to_listidx(subs1)
  subs2list <- mat_to_listidx(subs2)

  # apply set function to the lists
  subslist <- f(subs1list, subs2list)

  # convert back to a matrix
  list_to_matidx(subslist)
}


#' @rdname setops_subs
#' @keywords internal
union_subs <- function(subs1, subs2) setops_subs(subs1, subs2, union)

#' @rdname setops_subs
#' @keywords internal
intersect_subs <- function(subs1, subs2) setops_subs(subs1, subs2, intersect)
