#' Resolve speciation in a comrad community
#'
#' For each species, check if there is any gap `>= trait_dist_sp` in trait
#' values, and split the relevant species in two when one is found. The less
#' numerous half becomes the new species.
#'
#' @inheritParams default_params_doc
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

apply_speciation <- function(comm, trait_dist_sp = default_trait_dist_sp()) {

  # no NOTE
  z <- NULL # nolint
  species <- NULL # nolint
  ancestral_species <- NULL # nolint

  comrad::test_comrad_comm(comm)
  comrad::testarg_num(comm$z)
  comm <- comm[order(comm$z), ]

  spp <- unique(comm$species)

  for (sp in spp) {
    # Keep track of species members' position
    where <- which(comm$species == sp)
    # Extract members of focal species
    sp_members <- comm[comm$species == sp, ]
    nb_inds <- length(sp_members$z)

    # Check for gaps in trait values -------------------------------------------
    gaps <- comrad::find_trait_gaps_cpp(
      traits = sp_members$z,
      trait_dist_sp = trait_dist_sp
    )

    if (length(gaps) > 0) {
      gap <- gaps[1] # only the first gap is treated  for now (soft polytomy)

      # Split species in two ---------------------------------------------------
      new_sp <- NULL
      while (is.null(new_sp) || new_sp %in% spp) {
        new_sp <- charlatan::ch_hex_color()
      }
      sp_labels <- sp_members$species
      anc_labels <- sp_members$ancestral_species
      # Less numerous becomes new species
      if (gap < nb_inds / 2) {
        sp_labels[1:gap] <- new_sp
        anc_labels[1:gap] <- sp
      } else if (gap > nb_inds / 2) {
        sp_labels[(gap + 1):length(sp_labels)] <- new_sp
        anc_labels[(gap + 1):length(anc_labels)] <- sp
      } else { # same length, split at random
        # Flip a coin to determine which side of the gap becomes the new species
        coin_flip <- stats::rbinom(n = 1, size = 1, prob = 0.5)
        if (coin_flip == 1) {
          sp_labels[1:gap] <- new_sp
          anc_labels[1:gap] <- sp
        } else {
          sp_labels[(gap + 1):length(sp_labels)] <- new_sp
          anc_labels[(gap + 1):length(anc_labels)] <- sp
        }
      }

      # Test format & update community ---------------------------------
      comrad::testarg_char(sp_labels)
      comrad::testarg_length(sp_labels, length(sp_members$species))
      comm$ancestral_species[where] <- anc_labels
      comm$species[where] <- sp_labels
    }
  }
  return(comm)
}
