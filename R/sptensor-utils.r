#' Apply set function to subscript arrays
#'
#' @param subs1,subs2 subscript arrays
#' @param f set function to apply. Ex. union, setdiff, intersect
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
union_subs <- function(subs1, subs2) setops_subs(subs1, subs2, union)

#' @rdname setops_subs
intersect_subs <- function(subs1, subs2) setops_subs(subs1, subs2, intersect)
