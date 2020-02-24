rand_pop <- function() {
  n <- sample(1:100, 1)
  nb_species <- sample(1:5, 1)
  name_pool <- paste0(
    charlatan::ch_taxonomic_genus(nb_species), "_",
    charlatan::ch_taxonomic_epithet(nb_species)
  )
  pop <- tibble::tibble(
    z = stats::rnorm(n, 0, sd = default_carr_cap_width()),
    species = name_pool[sample(1:nb_species, n, replace = TRUE)],
    ancestral_species = rep(as.character(NA), n)
  )
  comrad::test_comrad_pop(pop)
  pop
}
