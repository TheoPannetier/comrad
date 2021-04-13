#' Names of DD models implemented in `comrad`
#'
#' @export
dd_model_names <- function() {
  c("lc", "ll", "lx", "xc", "xl", "xx", "xc2", "lx2", "xl2", "xx2")
}

#' Values of `DDD` argument `ddmodel` corresponding to `comrad` DD models
#'
#' @param dd_model_name character, the name of the DD model in comrad
#'
#' @return an integer code, value for this DD model in `DDD`
#' @author Theo Pannetier
#' @export
dd_model_comrad_to_ddd <- function(dd_model_name) {
  switch (dd_model_name,
    "lc" = 1,
    "ll" = 5,
    "lx" = 11,
    "lx2" = 6,
    "xc" = 9,
    "xl" = 13,
    "xx" = 12,
    "xc2" = 2,
    "xl2" = 8,
    "xx2" = 7
  )
}

#' Colours associated with each DD model
#'
#' For consistent plotting
#'
#' @export
dd_model_colours <- function() {
  dd_colours <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",  "#66A61E", "#E6AB02", "#A6761D", "#666666", "#E41A1C", "#377EB8")
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
    dd_model_xx(),
    dd_model_xc2(),
    dd_model_lx2(),
    dd_model_xl2(),
    dd_model_xx2()
  )
}
