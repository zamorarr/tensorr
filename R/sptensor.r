#' @rdname sptensor
#' @aliases sptensor,matrix,numeric,numeric-method
#' @export
setMethod("sptensor", c("matrix", "numeric", "numeric"), function(subs, vals, dims) {

  stopifnot(ncol(subs) == length(vals))
  stopifnot(nrow(subs) == length(dims))

  subs <- apply(subs, c(1,2), as.integer)
  dims <- as.integer(dims)

  methods::new("sptensor", subs = subs,  vals = vals, dims = dims)
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
setMethod("sptensor", c("list", "ANY", "numeric"), function(subs, vals, dims) {
  # number of dimensions
  ndims <- length(dims)
  index_len <- map_int(subs, length)

  stopifnot(length(subs) > 0)
  stopifnot(all(index_len == ndims))

  # convert list to matrix
  subs <- matrix(unlist(subs), nrow = ndims)

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
setMethod("dim", c(x = "sptensor"), function(x) x@dims )

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
    msg_subs <- NA
  } else {
    msg_subs <- apply(x@subs, 2, function(s) paste("<",paste(s,collapse=","),">",  sep =""))
  }

  cat("subs:", paste(msg_subs[1:width],collapse=" "))
  cat("\n")

  # values
  cat("vals:", x@vals[1:width])
})
