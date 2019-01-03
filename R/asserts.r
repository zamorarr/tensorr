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

