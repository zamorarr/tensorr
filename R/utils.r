# convert args i,j,... to array of coords.
# substitute missing args k with 1:dim[k]
build_indices <- function(dims, ...) {
  if (length(dims) != nargs()-1) {
    stop("num indices must match num dimensions", call. = FALSE)
  }

  # save calling environment
  penv <- parent.frame()

  # get arguments
  # args <- eval(substitute(alist(...)))
  args <- match.call(expand.dots = FALSE)$`...`

  # evaluate new args
  newargs <- map(seq_along(dims), function(k) {
    if (is.null(args[[k]])) seq_len(dims[k])
    else eval(args[[k]], penv)
  })

  # doesn't catch missing args first time...
  # need to fix this bug
  newargs <- map(seq_along(dims), function(k) {
    if (class(newargs[[k]]) == "name") seq_len(dims[k])
    else newargs[[k]]
  })

  # return grid as a matrix
  expand_indices(newargs)
}

expand_indices <- function(...) {
  res <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  res <- as.matrix(res)
  res <- t(res)
  apply(res, c(1,2), as.integer)
}
