#' Names of DD models implemented in `comrad`
#'
#' @export
dd_model_names <- function() {
  c("lc", "ll", "lx", "xc", "xl", "xx", "pc", "lp", "pl", "pp", "px", "xp")
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
    "pc" = 2,
    # "cl" = 3, # not used in comrad
    # "cp" = 4, # not used in comrad
    "ll" = 5,
    "lp" = 6,
    "pp" = 7,
    "pl" = 8,
    "xc" = 9,
    # "cx" = 10, # not used in comrad
    "lx" = 11,
    "xx" = 12,
    "xl" = 13,
    "xp" = 14,
    "px" = 15
  )
}

#' Colours associated with each DD model
#'
#' For consistent plotting
#'
#' @export
dd_model_colours <- function() {
  dd_colours <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",  "#66A61E", "#E6AB02", "#A6761D", "#666666", "#E41A1C", "#377EB8", "#5D99FD", "#FD5D99")
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
    "lc" = dd_model_lc(),
    "ll" = dd_model_ll(),
    "lx" = dd_model_lx(),
    "xc" = dd_model_xc(),
    "xl" = dd_model_xl(),
    "xx" = dd_model_xx(),
    "pc" = dd_model_pc(),
    "lp" = dd_model_lp(),
    "pl" = dd_model_pl(),
    "pp" = dd_model_pp(),
    "px" = dd_model_px(),
    "xp" = dd_model_xp()
  )
}
