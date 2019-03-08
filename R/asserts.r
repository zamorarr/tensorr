#' Are all values numeric?
#'
#' Checks if the input object only contains numeric values.
#' @param x An R object or expression.
#' @param .xname Not intended to be used directly.
#' @param severity How severe should the consequences of the assertion be?
#' Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @return \code{all_are_numeric} returns \code{TRUE} if \code{x} contains only
#' numeric values.The \code{assert_*} function throws an error on failure.
#'
#' @seealso \code{\link[base]{is.numeric}},
#' \code{\link[assertive.types]{is_numeric}}
#'
#' @importFrom assertive.base get_name_in_parent assert_engine
#' @keywords internal
all_are_numeric <- function(x, .xname = get_name_in_parent(x)) {
  all(map_lgl(x, is.numeric))
}

#' @rdname all_are_numeric
assert_all_are_numeric <- function(x, severity = getOption("assertive.severity", "stop")) {
  .xname <- get_name_in_parent(x)
  assert_engine(all_are_numeric, x, .xname = .xname,severity = severity,
                msg = paste(.xname, "contains non-numeric values"))
}

#' Are subscripts within dimensions?
#'
#' Checks that all subscripts have coordinates within the dimension sizes
#' @param subs subscripts
#' @param dims dimensions
#' @keywords internal
assert_subs_within_dims <- function(subs, dims) {
  # check subscipts within dimensions
  is_within_range <- apply(subs, 2, function(x) all(x <= dims))

  # if any aren't, throw an error message
  if (any(!is_within_range)) {
    dims_str <- paste(dims, collapse = " ")
    msg <- sprintf("dimensions of the tensor are %s but these subscripts are outside the range:", dims_str)

    for (i in which(!is_within_range)) {
      subs_str <- paste(subs[,i], collapse = " ")
      msg <- paste(msg, sprintf("[%s]: %s", i, subs_str), sep = "\n\t")
    }

    stop(msg, call. = FALSE)
  }
}

