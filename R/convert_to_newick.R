#' Convert a species history table to a Newick string
#'
#' Reads a tibble recording species birth, death and phylogenetic relations,
#' figures out the branch lenghts and return a phylogenetic tree in Newick
#' format. Based on [DDD::L2phylo()].
#'
#' @param phylo_tbl a table with phylogenetic information for each species, the
#' output of [assemble_phylo_tbl()]
#'
#' @author Théo Pannetier
#' @export

convert_to_newick <- function(phylo_tbl) {
  # Stupid but necessary for the build
  time_birth <- NULL

  newick_tbl <- phylo_tbl %>% dplyr::arrange(time_birth)

  while (nrow(newick_tbl) > 1) {

    tip_row <- max(
      which.max(newick_tbl$time_birth),
      2 # dirty hack to exclude the origin
    )
    child_name <- newick_tbl$species_name[tip_row]
    parent_name <- newick_tbl$ancestor_name[tip_row]
    parent_row <- grep(pattern = parent_name, x = newick_tbl$species_name)
    if (length(parent_row) > 1) {
      stop(
        "A string in the 'ancestor_name' column was matched by multiple rows."
      )
    }
    string_parent <- paste0(
      newick_tbl$species_name[parent_row], ":",
      newick_tbl$time_death[parent_row] - newick_tbl$time_birth[tip_row]
    )
    string_child <- paste0(
      child_name, ":",
      newick_tbl$time_death[tip_row] - newick_tbl$time_birth[tip_row]
    )
    newick_string <- paste0("(", string_parent, ",", string_child, ")")

    newick_tbl$species_name[parent_row] <- newick_string
    newick_tbl$time_death[parent_row] <- newick_tbl$time_birth[tip_row]
    newick_tbl <- newick_tbl[-tip_row, ]
  }

  newick_string <- paste0(
    newick_tbl$species_name, ":", newick_tbl$time_death, ";"
  )
  newick_string
}
