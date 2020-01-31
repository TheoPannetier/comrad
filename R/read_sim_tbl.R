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

read_sim_tbl <- function(path_to_file, skip = 16) {

  testarg_char(path_to_file)
  if (!grepl(".csv", path_to_file)) {
    stop("'path_to_file' must be a .csv")
  }

  sim_tbl <- readr::read_csv(
    path_to_file,
    skip = skip # skip metadata
    )
  # rm last row (total runtime)
  sim_tbl <- sim_tbl[-length(sim_tbl[[1]]), ]

  sim_tbl
}

