context("test-apply_speciation")

ancestral_species <- unique(default_init_pop()$species)

pop_mid_split_before <- default_init_pop()
pop_mid_split_before$z[6:10] <- 0.1
pop_mid_split_after <- pop_mid_split_before %>% apply_speciation()

pop_ext_split_before <- default_init_pop()
pop_ext_split_before$z[2:10] <- 0.1
pop_ext_split_after <- pop_ext_split_before %>% apply_speciation()

pop_mult_split_before <- default_init_pop()
pop_mult_split_before$z[4:6] <- 0.1
pop_mult_split_before$z[7:10] <- 0.2
pop_mult_split_after_one <- pop_mult_split_before %>% apply_speciation()
pop_mult_split_after_two <- pop_mult_split_after_one %>% apply_speciation()
first_sp <- pop_mult_split_after_two$species[1:3] %>% unique()
second_sp <- pop_mult_split_after_two$species[4:6] %>% unique()
third_sp <- pop_mult_split_after_two$species[7:10] %>% unique()

test_that("use", {
  # Output format
  expect_silent(pop_mid_split_before %>% apply_speciation())

  # Middle split
  expect_true(pop_mid_split_after$species[5] != pop_mid_split_after$species[6])
  expect_true(
    all.equal(
      pop_mid_split_before$z %>% sort(),
      pop_mid_split_after$z %>% sort()
    )
  )
  expect_equal(
    (pop_mid_split_after$species == ancestral_species) %>% sum(), 5
  )
  expect_equal(
    (pop_mid_split_after$ancestral_species == ancestral_species) %>%
      sum(na.rm = TRUE),
    5
  )
  # Extremity split
  expect_true(
    length(which(pop_ext_split_after$species == ancestral_species)) %in% c(1, 9)
  )

  # Multiple splits
  # 1st step
  # expect only one split to have occurred yet
  expect_equal(
    pop_mult_split_after_one$species %>% unique() %>% length(), 2
  )
  # 2nd step
  # assert splits have occurred at the expected positions
  expect_equal(
    first_sp %>% length(), 1
  )
  expect_equal(
    second_sp %>% length(), 1
  )
  expect_equal(
    third_sp %>% length(), 1
  )
  # Expect three different species
  expect_equal(
    c(first_sp, second_sp, third_sp) %>% unique() %>% length(), 3
  )


})

abnormal_pops <- lapply(1:10, function(x) default_init_pop())
abnormal_pops[[1]] <- stats::rnorm(10) # not a tibble
abnormal_pops[[2]] <- tibble::tibble(
  "z" = numeric(0),
  "species" = character(0),
  "ancestral_species" = character(0)
  ) # empty table
abnormal_pops[[3]][, 3] <- NULL # missing column
abnormal_pops[[4]]$z <- as.character(abnormal_pops[[4]]$z) # wrong type
abnormal_pops[[5]]$species <- as.factor(abnormal_pops[[5]]$species) # wrong type
abnormal_pops[[6]]$z[1] <- NA # NA in numeric
abnormal_pops[[7]]$z[1] <- NaN # boy do I hate those NaNs
abnormal_pops[[8]]$z[1:6] <- NaN # let's test twice, that's how much I hate them
colnames(abnormal_pops[[9]]) <- rep("", 3) # no names, rude.
abnormal_pops[[10]]$species[1] <- NA # NA in character

test_that("abuse", {
  expect_error(
    abnormal_pops[[1]] %>% apply_speciation(), "'pop' should be a tibble."
  )
  expect_error(
    abnormal_pops[[2]] %>% apply_speciation(), "'pop' is empty."
  )
  expect_error(
    abnormal_pops[[3]] %>% apply_speciation(), "'pop' should have 3 columns."
  )
  expect_error(
    abnormal_pops[[4]] %>% apply_speciation(),
    "'pop' column 'z' should be numeric."
  )
  expect_error(
    abnormal_pops[[5]] %>% apply_speciation(),
    "'pop' column 'species' should be a character."
  )
  expect_error(
    abnormal_pops[[6]] %>% apply_speciation(),
    "'traits' contains one or more NAs"
  )
  expect_error(
    abnormal_pops[[7]] %>% apply_speciation(),
    "'traits' contains one or more NaNs"
  )
  expect_error(
    abnormal_pops[[8]] %>% apply_speciation(),
    "'traits' contains one or more NaNs"
  )
  expect_error(
    abnormal_pops[[9]] %>% apply_speciation(),
    "'pop' should have columns 'z', 'species' and 'ancestral_species'."
  )
  expect_error(
    abnormal_pops[[10]] %>% apply_speciation(),
    "'pop' column 'species' contains one or more NAs."
  )
})

pre_node_random <- rand_pop()
post_node_random <- pre_node_random %>% apply_speciation()
nb_species_before <- pre_node_random$species %>% unique() %>% length()
nb_species_after <- post_node_random$species %>% unique() %>% length()

test_that("random", {
  expect_silent(pre_node_random %>% apply_speciation())
  # trait values should not change
  expect_true(all(sort(pre_node_random$z) == sort(post_node_random$z)))
  # no species should go extinct through the split
  expect_gte(nb_species_after, nb_species_before)
  # no more than one speciation event per species per generation
  expect_lte(nb_species_after, 2 * nb_species_before)
})
