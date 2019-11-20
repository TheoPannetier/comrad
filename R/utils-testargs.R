#' Assert a function argument is of correct type
#'
#' \itemize{
#'  \item \code{testarg_num()} asserts that the argument is a numeric value.
#'  Also rejects \code{NaNs} and \code{NAs} by default.
#'  \item \code{testarg_prop()} asserts that the argument value lies between 0
#'  and 1.
#'  \item \code{testarg_pos()} asserts that the argument is positive.
#'  \item \code{testarg_not_this()} asserts that the argument is different
#'  from one or several forbidden values.
#'  \item \code{testarg_char()} asserts that the argument is a character.
#'  \item \code{testarg_log()} asserts that the argument is a logical.
#'  \item \code{testarg_length()} asserts that the argument has the correct
#'  length.
#' }
#'
#' @param arg value of the asserted argument.
#'
#' @author Theo Pannetier
#'
#' @name testargs
NULL

#' @export
#' @rdname testargs
#' @param allow_nan logical, should NaNs pass the test?
#' @param allow_na logical, should NAs pass the test?
testarg_num <- function(arg, allow_nan = FALSE, allow_na = FALSE) {
  if (!is.numeric(arg)) {
    stop("'", substitute(arg), "' must be numeric")
  } else if (!allow_nan && any(is.nan(arg))) {
    stop("'", substitute(arg), "' contains one or more NaNs")
  } else if (!allow_na && any(is.na(arg))) {
    stop("'", substitute(arg), "' contains one or more NAs")
  }
}

#' @export
#' @rdname testargs
testarg_pos <- function(arg) {
  if (any(arg < 0)) {
    stop("'", substitute(arg), "' must be a positive numeric")
  }
}

#' @export
#' @rdname testargs
testarg_prop <- function(arg) {
  if (any(!dplyr::between(arg, 0, 1))) {
    stop("'", substitute(arg), "' must be a numeric between 0 and 1")
  }
}

#' @export
#' @rdname testargs
#' @param forbidden a vector containing forbidden values.
#' @details Currenty \code{testarg_not_this()} cannot be tested properly.
testarg_not_this <- function(arg, forbidden) {
  if (any(arg %in% forbidden)) {
    stop("'", substitute(arg), "' contains forbidden values: ",
         paste(intersect(arg, forbidden), collapse = " "))
  }
}

#' @export
#' @rdname testargs
testarg_char <- function(arg) {
  if (!is.character(arg)) {
    stop("'", substitute(arg), "' must be a character.")
  }
}

#' @export
#' @rdname testargs
testarg_log <- function(arg) {
  if (!is.logical(arg)) {
    stop("'", substitute(arg), "' must be a logical")
  }
}

#' @export
#' @rdname testargs
#' @param correct_length numeric, the length the argument should have.
testarg_length <- function(arg, correct_length) {
  if (length(arg) != correct_length) {
    stop("'", substitute(arg), "' must have length ", correct_length)
  }
}
