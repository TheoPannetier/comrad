run_generation_step <- function(
  traits_pop,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  mutation_sd = default_mutation_sd(),
  fitness_func = fitness_func_positive_logistic
  ) {

  # Test argument type ---------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(growth_rate)
  testarg_pos(growth_rate)
  testarg_num(comp_width)
  testarg_pos(comp_width)
  testarg_num(trait_opt)
  testarg_num(carr_cap_opt)
  testarg_pos(carr_cap_opt)
  testarg_num(carr_cap_width)
  testarg_pos(carr_cap_width)
  testarg_num(mutation_sd)
  testarg_pos(mutation_sd)

  # Compute fitnesses
  fitness_pop <- get_fitness(
    traits_pop = traits_pop,
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

  next_gen_traits <- create_next_gen_traits(
    traits_pop = traits_pop,
    nb_offspring_pop = nb_offspring_pop,
    mutation_sd = mutation_sd
  )
  next_gen_traits
}
