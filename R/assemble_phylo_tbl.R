assemble_phylo_tbl <- function(comrad_tbl) {
  # Stupid but necessary for the build
  species <- NULL
  ancestral_species <- NULL

  phylo_tbl <- tibble::tibble(
    "species_name" = character(),
    "ancestor_name" = character(),
    "time_birth" = numeric(),
    "time_death" = numeric()
  )

  for (sp in unique(comrad_tbl$species)) {

    time_birth <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(t) %>%
      unlist() %>%
      min()
    time_death <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(t) %>%
      unlist() %>%
      max()
    ancestor_name <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(ancestral_species) %>%
      unlist() %>%
      unique()

    sp_entry <- tibble::tibble(
      "species_name" = sp,
      "ancestor_name" = ancestor_name,
      "time_birth" = time_birth,
      "time_death" = time_death
    )

    phylo_tbl <- rbind(phylo_tbl, sp_entry)
  }
  phylo_tbl

}
