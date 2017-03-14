#' @rdname unfolded_sptensor
#' @aliases unfolded_sptensor,sparseMatrix,numeric,numeric-method
#' @export
#' @importClassesFrom Matrix Matrix
#' @importFrom assertive.base assert_are_identical
setMethod("unfolded_sptensor", c("Matrix", "numeric", "numeric"), function(mat, mode, tensor_dims) {
  # coerce doubles to int
  tensor_dims <- as.integer(tensor_dims)
  mode <- as.integer(mode)

  # check dimensions
  len_tensor <- as.integer(prod(tensor_dims))
  assert_are_identical(length(mat), len_tensor)
  assert_are_identical(nrow(mat), tensor_dims[mode])

  # convert mat to COO format
  mat <- methods::as(mat, "TsparseMatrix")

  methods::new("unfolded_sptensor",
               mat = mat, mode = mode, tensor_dims = tensor_dims)
})

#' @rdname unfold
#' @aliases unfold,sptensor,numeric-method
#' @export
setMethod("unfold", signature(x = "sptensor", mode = "numeric"), function(x, mode) {
  mat <- matricize_sptensor(x, mode)
  unfolded_sptensor(mat, mode, dim(x))
})

matricize_sptensor <- function(x, mode) {
  subs <- nzsubs(x)
  vals <- nzvals(x)
  dims <-  dim(x)

  newindices <- unfold_indices(mode, subs, vals, dims)
  i <- newindices$i
  j <- newindices$j
  newdims <- newindices$newdims

  Matrix::sparseMatrix(i = i, j = j, x = vals, dims = newdims, giveCsparse = FALSE)
}

#' @rdname refold
#' @aliases refold,unfolded_sptensor-method
#' @importClassesFrom Matrix TsparseMatrix
#' @export
setMethod("refold", "unfolded_sptensor", function(x) {
  mat <- methods::as(x@mat, "TsparseMatrix")

  vals <- mat@x
  dims <- x@tensor_dims
  subs <- refold_indices(mat@i + 1L, mat@j + 1L, x@mode, dims)

  sptensor(subs, vals, dims)
})

unfold_indices <- function(mode, subs, vals, dims) {
  # calculate new dims
  #newdims <- c(dims[mode], reduce(dims[-mode],`*`))
  newdims <- c(dims[mode], prod(dims[-mode]))

  # calculate i
  i <- subs[mode,,drop = TRUE]

  #calculate j
  J <- cumprod(dims[-mode])
  J <- c(1, J[-length(J)])
  ik <- subs[-mode,,drop = FALSE]
  j <- 1 + colSums((ik - 1)*J)

  list(i = i, j = j,newdims = newdims)
}

refold_indices <- function(i, j, mode, dims) {
  index_list <- map2(i,j, ~fiber_coords(.x, .y, mode, dims))
  list_to_matidx(index_list)
}

fiber_coords <- function(fillval, fiber_num, mode, dims) {
  coords <- array_index(fiber_num, dims[-mode])

  if (mode == 1) {
    rbind(fillval,coords)
  } else if (mode == length(dims)) {
    rbind(coords, fillval)
  }
  else {
    rbind(coords[1:(mode-1),],fillval,coords[mode:nrow(coords),])
  }
}

#' @rdname show
setMethod("show", "unfolded_sptensor", function(object) {
  x <- object

  # header message
  msg_dims <- paste(x@tensor_dims, "x", sep = "", collapse = "")
  msg_dims <- strtrim(msg_dims, nchar(msg_dims)-1)
  msg <- paste("<A", msg_dims,"unfolded sparse tensor along mode", x@mode,">", sep = " " )
  cat(msg)
  cat("\n")

  print(x@mat)
})
