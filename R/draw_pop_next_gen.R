#' Draw a new population from the current one
#'
#' Based on the current population traits, compute the fitness, draw
#' offspring, apply mutations and speciation events were relevant.
#'
#' @inheritParams default_params_doc
#'
#' @author Théo Pannetier
#' @export

draw_pop_next_gen <- function(
  pop,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd()
) {

  # Test argument type ---------------------------------------------------------
  comrad::test_comrad_pop(pop)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carr_cap_opt)
  comrad::testarg_pos(carr_cap_opt)
  comrad::testarg_num(carr_cap_width)
  comrad::testarg_pos(carr_cap_width)
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Compute fitnesses
  fitness_pop <- comrad::get_fitness(
    traits_pop = pop$z,
    growth_rate = growth_rate,
    comp_width = comp_width,
    trait_opt = trait_opt,
    carr_cap_opt = carr_cap_opt,
    carr_cap_width = carr_cap_width
  )
  comrad::testarg_not_this(fitness_pop, Inf)

  # Create next generation from parent fitness ---------------------------------
  nb_offspring_pop <- comrad::draw_nb_offspring(fitness = fitness_pop)
  comrad::testarg_length(nb_offspring_pop, length(pop$z))

  new_pop <- tibble::tibble(
    "z" = rep(pop$z, nb_offspring_pop),
    "species" = rep(pop$species, nb_offspring_pop),
    "ancestral_species" = rep(pop$ancestral_species, nb_offspring_pop)
    # new pop inherits traits and species from parents
  )
  comrad::testarg_length(new_pop$z, sum(nb_offspring_pop))

  # Catch extinction -----------------------------------------------------------
  if (length(new_pop$species) < 1) {
    return(new_pop)
  }

  # Draw and apply mutations ---------------------------------------------------
  new_pop$z <- comrad::apply_mutations(
    traits_pop = new_pop$z,
    prob_mutation = prob_mutation,
    mutation_sd = mutation_sd
  )

  # Resolve speciation ---------------------------------------------------------
  new_pop <- comrad::apply_speciation(
    pop = new_pop
  )
  comrad::test_comrad_pop(new_pop)

  new_pop
}
