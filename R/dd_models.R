#' Names of DD models implemented in `comrad`
#'
#' @export
#'
dd_model_names <- function() {
  c("lc", "ll", "lx","xc", "xl", "xx")
}

#' Colours associated with each DD model
#'
#' For consistent plotting
#'
#' @export
dd_model_colours <- function() {
  dd_colours <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",  "#66A61E", "#E6AB02")
  names(dd_colours) <- dd_model_names()
  return(dd_colours)
}

#' List of all diversity-dependent models
#'
#' Shortcut function to call all DD function objects
#'
#' @export
dd_models <- function() {
  list(
    dd_model_lc(),
    dd_model_ll(),
    dd_model_lx(),
    dd_model_xc(),
    dd_model_xl(),
    dd_model_xx()
  )
}
