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

  # species must be ordered by chronological order
  newick_tbl <- phylo_tbl[order(phylo_tbl$time_birth), ]

  # Concatenate Newick string recursively
  while (nrow(newick_tbl) > 1) {

    # Process tips from youngest to oldest
    child <- which.max(newick_tbl$time_birth)
    child <- max(child, 2) # skip the MRCA
    child_name <- newick_tbl$species_name[child]

    parent_name <- newick_tbl$ancestor_name[child]

    # Look for tip/clade containing the parent
    parent <- grep(
      pattern = parent_name,
      x = newick_tbl$species_name
    )

    string_parent <- paste0(
      newick_tbl$species_name[parent], ":",
      newick_tbl$time_death[parent] - newick_tbl$time_birth[child]
    )
    string_child <- paste0(
      child_name, ":",
      newick_tbl$time_death[child] - newick_tbl$time_birth[child]
    )
    newick_string <- paste0("(", string_parent, ",", string_child, ")")

    newick_tbl$species_name[parent] <- newick_string
    newick_tbl$time_death[parent] <- newick_tbl$time_birth[child]
    newick_tbl <- newick_tbl[-child, ]
  }

  newick_string <- paste0(
    "(", newick_tbl$species_name, ":", newick_tbl$time_death, ");"
  )
  newick_string
}
