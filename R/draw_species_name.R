#' Draw a made-up name for a species
#'
#' Draw a random species name and combines it with the supplied genus.
#'
#' @param genus_name character, a genus name.
#'
#' @author Théo Pannetier
#' @export

draw_species_name <- function(genus_name = "Haggis") {
  species_name <- charlatan::ch_taxonomic_epithet()
  paste0(genus_name, "_", species_name)
}
