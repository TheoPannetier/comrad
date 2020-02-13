#' Read simulation data
#'
#' Reads a `.csv` file as produced by [run_simulation()] and returns the parsed
#' table it contains.
#'
#' @param path_to_file character, path to the `.csv` file.
#' @param skip numeric, number of lines (of metadata) to skip. Passed to
#' [readr::read_csv()].
#'
#' @author Théo Pannetier
#' @export

read_comrad_tbl <- function(path_to_file, skip = 17) {

  testarg_char(path_to_file)
  if (!grepl(".csv", path_to_file)) {
    stop("'path_to_file' must be a .csv")
  }

  comrad_tbl <- readr::read_csv(
    path_to_file,
    skip = skip, # skip metadata
    col_types = readr::cols(
      readr::col_number(),
      readr::col_number(),
      readr::col_character(),
      readr::col_character(),
      readr::col_number()
    )
  )
  comrad_tbl
}
