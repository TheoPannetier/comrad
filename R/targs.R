#' Assert a function argument is of correct type
#'
#' \itemize{
#'  \item \code{targ_num()} asserts that the argument is a numeric value.
#'  \item \code{targ_pos()} asserts that the argument is a numeric value and
#'  positive.
#'  \item \code{targ_not_zero()} asserts that the argument is a numeric value
#'   different from 0
#'  \item \code{targ_char()} asserts that the argument is a character.
#'  \item \code{targ_log()} asserts that the argument is a logical.
#'  \item \code{targ_length()} asserts that the argument has the correct length.
#' }
#'
#' @param arg_val value of the asserted argument.
#' @param arg_name name of the asserted argument to be passed to an error message
#'
#' @author Theo Pannetier
#'
#' @name targs
NULL

#' @export
#' @rdname targs
targ_num <- function(arg_val, arg_name) {
  if (!is.numeric(arg_val)) {
    stop("'", arg_name, "' must be numeric.")
  }
}

#' @export
#' @rdname targs
targ_pos <- function(arg_val, arg_name) {
  targ_num(arg_val, arg_name)
  if (arg_val < 0) {
    stop("'", arg_name, "' must be a positive numeric.")
  }
}

#' @export
#' @rdname targs
targ_prop <- function(arg_val, arg_name) {
  targ_num(arg_val, arg_name)
  if (!dplyr::between(arg_val, 0, 1)) {
    stop("'", arg_name, "' must be a numeric between 0 and 1")
  }
}

#' @export
#' @rdname targs
targ_not_zero <- function(arg_val, arg_name) {
  targ_num(arg_val, arg_name)
  if (arg_val == 0) {
    stop("'", arg_name, "' must be a numeric different from 0")
  }
}

#' @export
#' @rdname targs
targ_char <- function(arg_val, arg_name) {
  if (!is.character(arg_val)) {
    stop("'", arg_name, "' must be a character.")
  }
}

#' @export
#' @rdname targs
targ_log <- function(arg_val, arg_name) {
  if (!is.logical(arg_val)) {
    stop("'", arg_name, "' must be a logical")
  }
}

#' @export
#' @rdname targs
#' @param correct_length numeric, the length the argument should have.
targ_length <- function(arg_val, arg_name, correct_length) {
  if (length(arg_val) != correct_length) {
    stop("'", arg_name, "' must have length ", correct_length)
  }
}
