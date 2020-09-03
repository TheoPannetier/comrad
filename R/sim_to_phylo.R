#' Convert `comrad` simulation results into a phylogeny
#'
#' Reads a table of results and returns a `phylo` class object.
#'
#' @inheritParams default_params_doc
#' @param with_extinct logical, should extinct taxa be included? `TRUE` returns
#' the full tree, `FALSE` returns the reconstructed tree.
#'
#' @author Théo Pannetier
#' @export

sim_to_phylo <- function(comrad_tbl, with_extinct = TRUE) {
  comrad_tbl %>%
    dplyr::select("z", "species", "ancestral_species") %>%
    comrad::test_comrad_comm()

  newick_str <- comrad_tbl %>%
    comrad::build_spp_tbl() %>%
    comrad::write_newick_str()

  phylo <- ape::read.tree(text = newick_str)

  if (!with_extinct) {
    phylo <- ape::drop.fossil(phylo)
  }
  phylo
}
