
testthat::test_that("phylo builds", {
  #
  # sp1 >---------------------> sp1
  #
  exptd_phylo <- ape::read.tree(text = "(sp1:1000);")
  comrad_tbl <- tibble::tibble(
    "t" = 1:1000,
    "z" = 0,
    "species" = "sp1",
    "ancestral_species" = as.character(NA)
  )
  testthat::expect_silent(
    phylo <- comrad_tbl %>% convert_sim_to_phylo()
  )
  testthat::expect_equal(
    phylo, exptd_phylo
  )

  #                |-----------> sp1
  # sp1 >----------|
  #                |-----------> sp2

  exptd_phylo <- ape::read.tree(text = "((sp1:500, sp2:500)sp1:500);")
  comrad_tbl <- tibble::tibble(
    "t" = c(1:1000, 501:1000),
    "z" = 0,
    "species" = c(rep("sp1", 1000), rep("sp2", 500)),
    "ancestral_species" = c(rep(NA, 1000), rep("sp1", 500))
  )
  testthat::expect_silent(
    phylo <- comrad_tbl %>% convert_sim_to_phylo()
  )
  testthat::expect_equal(
    phylo, exptd_phylo
  )

  # Anc. species replaced by daughter
  #                |----X sp1
  # sp1 >----------|
  #                |-----------> sp2
  #
  exptd_phylo <- ape::read.tree(text = "((sp1:250, sp2:500)sp1:500);")
  comrad_tbl <- tibble::tibble(
    "t" = c(1:750, 501:1000),
    "z" = 0,
    "species" = c(rep("sp1", 750), rep("sp2", 500)),
    "ancestral_species" = c(rep(NA, 750), rep("sp1", 500))
  )
  testthat::expect_silent(
    phylo <- comrad_tbl %>% convert_sim_to_phylo()
  )
  testthat::expect_equal(
    phylo, exptd_phylo
  )
})

testthat::test_that("trait distribution shouldn't matter", {
  if (Sys.getenv("TRAVIS") != "") {
    skip("only run on CI")
  } else {

    #                 |------> sp1
    #           |-----|sp1
    #           |     |------> sp3
    # sp1 >-----|sp1
    #           |     |------>  sp2
    #           |-----|sp2
    #                 |--x sp4
    #
    # with 50 inds/sp/gen & randomly distributed traits

    exptd_phylo <- ape::read.tree(
      text = "(((sp1:500,sp3:500)sp1:250,(sp2:500,sp4:250)sp2:250)sp1:500);"
    )
    gen_seq <- c(
      rep(1:1000, rep(50, 1000)),   # sp1
      rep(251:1000, rep(50, 750)),  # sp2
      rep(501:1000, rep(50, 500)),  # sp3
      rep(501:750, rep(50, 250))    # sp4
    )
    spp_seq <- c(
      rep("sp1", 50000),
      rep("sp2", 37500),
      rep("sp3", 25000),
      rep("sp4", 12500)
    )
    anc_spp_seq <- c(
      rep(as.character(NA), 50000),
      rep("sp1", 37500),
      rep("sp1", 25000),
      rep("sp2", 12500)
    )

    comrad_tbl <- tibble::tibble(
      "t" = as.numeric(gen_seq),
      "z" = as.numeric(NA), # values drawn below
      "species" = spp_seq,
      "ancestral_species" = anc_spp_seq
    )

    purrr::walk(
      1:100,
      function(x) {

        # Draw random values for traits
        comrad_tbl$z <- rnorm(length(gen_seq), 0, 1)
        comrad_tbl %>%
          dplyr::bind_rows(tibble::tibble(
            "t" = 0,
            "z" = rnorm(10, 0, 1), # values drawn below
            "species" = "sp1",
            "ancestral_species" = NA
          )) %>%
          plot_comm_trait_evolution(hex_fill = "counts")

        testthat::expect_silent(
          phylo <- comrad_tbl %>% convert_sim_to_phylo()
        )
        testthat::expect_equal(
          phylo, exptd_phylo
        )
      }
    )
  }
})

