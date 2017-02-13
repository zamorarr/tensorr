#' @rdname dtensor
#' @aliases dtensor,array-method
#' @export
setMethod("dtensor", "array", function(x) methods::new("dtensor", x = x) )

#' @rdname dtensor
#' @aliases dtensor,numeric-method
#' @export
setMethod("dtensor", "numeric", function(x) {
  if (length(x) == 0) stop("Dimensions cannot be zero", call. = FALSE)
  methods::new("dtensor", x = matrix(x, nrow = length(x)))

})

#' @describeIn is_tensor dense tensor
#' @export
is_dtensor <- function(x) inherits(x, "dtensor")

#' @rdname dim
#' @aliases dim,dtensor-method
#' @export
setMethod("dim", "dtensor", function(x) dim(x@x))

#' @rdname nzsubs
#' @aliases nzsubs,dtensor-method
#' @export
setMethod("nzsubs", "dtensor", function(x) array_index(which(x@x != 0), dim(x)))

#' @rdname nzvals
#' @aliases nzvals,dtensor-method
#' @export
setMethod("nzvals", "dtensor", function(x) x[which(x@x != 0)])

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
