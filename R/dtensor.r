#' @rdname dtensor
#' @aliases dtensor,array-method
#' @export
setMethod("dtensor", "array", function(x) {
  res <- methods::new("dtensor", x = x)

  # empty dimnames
  dims <- dim(res)
  empty_dimnames <- vector("list", length(dims))
  dimnames(res) <- empty_dimnames

  res
})

#' @rdname dtensor
#' @aliases dtensor,numeric-method
#' @export
#' @importFrom assertive.properties assert_is_non_empty
setMethod("dtensor", "numeric", function(x) {
  assert_is_non_empty(x)
  methods::new("dtensor", x = matrix(x, nrow = length(x)))
})

#' @describeIn is_tensor dense tensor
#' @export
is_dtensor <- function(x) inherits(x, "dtensor")

#' @rdname dim
#' @aliases dim,dtensor-method
#' @export
setMethod("dim", "dtensor", function(x) dim(x@x))

#' @rdname dimnames
#' @aliases dimnames,dtensor-method
#' @export
setMethod("dimnames", c(x = "dtensor"), function(x) dimnames(x@x))

#' @rdname dimnames
#' @aliases dimnames<-,dtensor-method
#' @export
setMethod("dimnames<-", c(x = "dtensor", value = "list"), function(x, value) {
  dimnames(x@x) <- value
  x
})

#' @rdname dimnames
#' @aliases dimnames<-,dtensor-method,NULL
#' @export
setMethod("dimnames<-", c(x = "dtensor", value = "NULL"), function(x, value) {
  warning("dimnames cannot be NULL. converting to a list of NULL values", call. = FALSE)
  dims <- dim(x)
  NULL_dimnames <- vector("list", length(dims))

  dimnames(x@x) <- NULL_dimnames
  x
})

#' @rdname dimnames
#' @aliases dimnames<-,dtensor-method,ANY
#' @export
setMethod("dimnames<-", c(x = "dtensor", value = "ANY"), function(x, value) {
  stop("dimnames must be a list of length equal to the length of the dim(x)", call. = FALSE)
})


#' @rdname nzsubs
#' @aliases nzsubs,dtensor-method
#' @export
setMethod("nzsubs", "dtensor", function(x) array_index(which(x@x != 0), dim(x)))

#' @rdname zsubs
#' @aliases zsubs,dtensor-method
#' @keywords internal
setMethod("zsubs", "dtensor", function(x) array_index(which(x@x == 0), dim(x)))

#' @rdname allsubs
#' @aliases allsubs,dtensor-method
#' @keywords internal
setMethod("allsubs", "dtensor", function(x) array_index(seq_along(x), dim(x)))

#' @rdname nzvals
#' @aliases nzvals,dtensor-method
#' @export
setMethod("nzvals", "dtensor", function(x) x[nzsubs(x)])

#' @rdname show
setMethod("show", "dtensor", function(object) {
  x <- object
  # header message
  msg_dims <- paste(dim(x), "x", sep = "", collapse = "")
  msg_dims <- strtrim(msg_dims, nchar(msg_dims)-1)
  msg <- paste("<A", msg_dims,"dense tensor>", sep = " " )
  cat(msg)
  cat("\n")

  # subscripts
  print(x@x)
})
