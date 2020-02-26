#' Resolve speciation in a comrad community
#'
#' For each species, check if there is any gap of `>=` 0.1 in trait values, and
#' split the relevant species in two when one is found.
#'
#' @inheritParams default_params_doc
#'
#' @note `apply_speciation()` can currently only split one species into two. If
#' multiple gaps emerge in a species at a single time step, only the first one
#' will be treated. As long as branching does not happen at every generation,
#' this is unlikely to be an issue, because an unresolved gap will be caught on
#' the next time step. In phylogenetic terms, this results in polytomies being
#' resolved as soft polytomies.
#' Besides, simultaneous branching events in different species are handled
#' perfectly fine.
#'
#' @author Théo Pannetier
#' @export

apply_speciation <- function(comm) {
  # Stupid but necessary for the build
  z <- NULL
  species <- NULL
  ancestral_species <- NULL

  comrad::test_comrad_comm(comm)

  comm <- comm %>% dplyr::arrange(z)

  for (sp in unique(comm$species)) {
    # Keep track of species members' position
    where <- which(comm$species == sp)
    # Extract members of focal species
    sp_members <- comm %>% dplyr::filter(species == sp)

    # Check for gaps in trait values -------------------------------------------
    gaps <- sp_members %>%
      dplyr::select(z) %>%
      unlist() %>%
      find_trait_gaps()

    if (length(gaps) > 0) {
      gap <- gaps[1] # only the first gap is treated (soft polytomy)
      # Split species in two ---------------------------------------------------

      # Flip a coin to determine which side of the gap becomes the new species
      coin_flip <- stats::rbinom(n = 1, size = 1, prob = 0.5)

      # for (i in seq_along(gaps)) {
      new_sp <- charlatan::ch_hex_color()
      sp_labels <- sp_members %>%
        dplyr::select(species) %>%
        unlist()
      anc_labels <- sp_members %>%
        dplyr::select(ancestral_species) %>%
        unlist()
      if (coin_flip == 1) {
        sp_labels[1:gap] <- new_sp
        anc_labels[1:gap] <- sp
      } else {
        sp_labels[(gap + 1):length(sp_labels)] <- new_sp
        anc_labels[(gap + 1):length(anc_labels)] <- sp

      }
      # }
      # Test format & update community ---------------------------------
      comrad::testarg_char(sp_labels)
      comrad::testarg_length(sp_labels, length(sp_members$species))
      comm$ancestral_species[where] <- anc_labels
      comm$species[where] <- sp_labels
    }
  }
  comrad::test_comrad_comm(comm)
  return(comm)
}
