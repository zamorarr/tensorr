#' @rdname sptensor
#' @aliases sptensor,matrix,ANY,numeric-method
#' @export
#' @importFrom assertive.base assert_are_identical
setMethod("sptensor", c("matrix", "ANY", "numeric"), function(subs, vals, dims) {

  # dimensions should match
  assert_are_identical(ncol(subs), length(vals))
  assert_are_identical(nrow(subs), length(dims))

  # coerce subscripts and dimensions to integer
  subs <- apply(subs, c(1,2), as.integer)
  dims <- as.integer(dims)

  # if any vals are zero, don't include them
  nonzeros <- which(vals != 0)
  subs <- subs[, nonzeros, drop = FALSE]
  vals <- vals[nonzeros]

  # ascending order subscripts
  ascending_indices <- order(vec_index(subs, dims))
  subs <- subs[, ascending_indices, drop = FALSE]
  vals <- vals[ascending_indices]

  # drop duplicated subscripts?
  duped <- duplicated(subs, MARGIN = 2)
  subs <- subs[, !duped, drop = FALSE]
  vals <- vals[!duped]

  # empty dimnames
  empty_dimnames <- vector("list", length(dims))

  # check that subscripts are within dim range
  assert_subs_within_dims(subs, dims)

  methods::new("sptensor", subs = subs,  vals = vals, dims = dims, dimnames = empty_dimnames)
})

#' @rdname sptensor
#' @aliases sptensor,matrix,missing,numeric-method
#' @export
setMethod("sptensor", c("matrix", "missing", "numeric"), function(subs, vals, dims) {
  vals = rep(1, ncol(subs))
  sptensor(subs, vals, dims)
})

#' @rdname sptensor
#' @aliases sptensor,list,ANY,numeric-method
#' @export
#' @importFrom assertive.properties assert_is_non_empty assert_are_same_length
setMethod("sptensor", c("list", "ANY", "numeric"), function(subs, vals, dims) {
  assert_is_non_empty(subs)
  walk(subs, ~assert_are_same_length(.x, dims))

  # convert subs list to matrix
  subs <- list_to_matidx(subs)

  # create sptensor
  if (missing(vals)) sptensor(subs, dims = dims)
  else sptensor(subs, vals, dims = dims)
})


# identity

#' @describeIn is_tensor sparse tensor
#' @export
is_sptensor <- function(x) inherits(x, "sptensor")

#' @rdname dim
#' @aliases dim,sptensor-method
#' @export
setMethod("dim", c(x = "sptensor"), function(x) x@dims)

#' @rdname dimnames
#' @aliases dimnames,sptensor-method
#' @export
setMethod("dimnames", c(x = "sptensor"), function(x) x@dimnames)

#' @rdname dimnames
#' @aliases dimnames<-,sptensor-method,list
#' @export
setMethod("dimnames<-", c(x = "sptensor", value = "list"), function(x, value) {
  # ensure provided dimnames match dimensions of tensor
  dims <- dim(x)
  assertive.properties::assert_is_of_length(value, length(dims))


  dims_value <- vapply(value, length, integer(1L))
  if(!all(dims_value == dims | dims_value == 0)) {
    stop("dimnames components must have same length as dimensions of tensor", call. = FALSE)
  }

  x@dimnames <- value
  x
})

#' @rdname dimnames
#' @aliases dimnames<-,sptensor-method,NULL
#' @export
setMethod("dimnames<-", c(x = "sptensor", value = "NULL"), function(x, value) {
  warning("dimnames cannot be NULL. converting to a list of NULL values", call. = FALSE)
  dims <- dim(x)
  NULL_dimnames <- vector("list", length(dims))

  x@dimnames <- NULL_dimnames
  x
})

#' @rdname dimnames
#' @aliases dimnames<-,sptensor-method,ANY
#' @export
setMethod("dimnames<-", c(x = "sptensor", value = "ANY"), function(x, value) {
  stop("dimnames must be a list of length equal to the length of the dim(X)", call. = FALSE)
})

#' @rdname nzsubs
#' @aliases nzsubs,sptensor-method
#' @export
setMethod("nzsubs", "sptensor", function(x) x@subs)

#' @rdname zsubs
#' @aliases zsubs,sptensor-method
#' @keywords internal
setMethod("zsubs", "sptensor", function(x) {
  allsubslist <- mat_to_listidx(allsubs(x))
  nzsubslist <- mat_to_listidx(nzsubs(x))

  zsubslist <- setdiff(allsubslist, nzsubslist)

  if (length(zsubslist) > 0 )list_to_matidx(zsubslist)
  else matrix(numeric(0), nrow = length(dim(x)))

})

#' @rdname allsubs
#' @aliases allsubs,sptensor-method
#' @keywords internal
setMethod("allsubs", "sptensor", function(x) {
  array_index(seq_along(x), dim(x))
})


#' @rdname nzvals
#' @aliases nzvals,sptensor-method
#' @export
setMethod("nzvals", "sptensor", function(x) x@vals)

#' @rdname show
setMethod("show", c(object =  "sptensor"), function(object) {
  x <- object
  width <- min(5L, length(x@vals))

  # header message
  msg_dims <- paste(dim(x), "x", sep = "", collapse = "")
  msg_dims <- strtrim(msg_dims, nchar(msg_dims)-1)
  msg <- paste("<A", msg_dims,"sparse tensor with",length(x@vals),"non-zero entries>", sep = " " )
  cat(msg)
  cat("\n")

  # subscripts
  if (all(is.na(x@subs))) {
    msg_subs <- "<empty>"
  } else {
    msg_subs <- apply(x@subs, 2, function(s) paste("<",paste(s,collapse=","),">",  sep =""))
    if (length(msg_subs) > width) {
      msg_subs <- c(msg_subs[1:width], "...")
    }
  }

  cat("subs:", msg_subs)
  cat("\n")

  # values
  if (length(x@vals) == 0) {
    msg_vals <- "<empty"
  }
  else {
    msg_vals <- x@vals
    if (length(msg_vals) > width) {
      msg_vals <- c(msg_vals[1:width], "...")
    }
  }
  cat("vals:", msg_vals)
})
