draw_next_gen <- function(
  pop,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd(),
  fitness_func = fitness_func_ricker
  ) {

  # Test argument type ---------------------------------------------------------
  test_comrad_pop(pop)
  testarg_num(growth_rate)
  testarg_pos(growth_rate)
  testarg_num(comp_width)
  testarg_pos(comp_width)
  testarg_num(trait_opt)
  testarg_num(carr_cap_opt)
  testarg_pos(carr_cap_opt)
  testarg_num(carr_cap_width)
  testarg_pos(carr_cap_width)
  testarg_num(prob_mutation)
  testarg_prop(prob_mutation)
  testarg_num(mutation_sd)
  testarg_pos(mutation_sd)

  # Compute fitnesses
  fitness_pop <- get_fitness(
    traits_pop = pop$z,
    growth_rate = growth_rate,
    comp_width = comp_width,
    trait_opt = trait_opt,
    carr_cap_opt = carr_cap_opt,
    carr_cap_width = carr_cap_width,
    fitness_func = fitness_func
  )
  testarg_not_this(fitness_pop, Inf)

  # Create next generation from parent fitness ---------------------------------
  nb_offspring_pop <- draw_nb_offspring(fitness = fitness_pop)
  testarg_length(nb_offspring_pop, length(pop$z))

  new_pop <- tibble::tibble(
    "z" = rep(pop$z, nb_offspring_pop),
    "species" = rep(pop$species, nb_offspring_pop)
    # new pop inherits traits and species from parents
  )

  # Catch extinction -----------------------------------------------------------
  if (length(new_pop$species) < 1) {
    return(new_pop)
  }

  # Draw and apply mutations ---------------------------------------------------
  new_pop$z <- apply_mutations(
    traits_pop = new_pop$z,
    prob_mutation = prob_mutation,
    mutation_sd = mutation_sd
  )

  # Resolve speciation ---------------------------------------------------------
  new_pop <- apply_speciation(
    pop = new_pop
  )
  test_comrad_pop(new_pop)

  new_pop
}
