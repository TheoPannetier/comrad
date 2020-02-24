time_to_speciation <- function(phylo, time_origin) {

  ltt_table <- phylo %>% ape::ltt.plot.coords() %>% tibble::as_tibble()

  #¯\_(ツ)_/¯
  ltt_table <- ltt_table[-nrow(ltt_table), ]
  ltt_table$N <- ltt_table$N + 1 # nolint
  ltt_table <- rbind(list("time" = time_origin, "N" = 0), ltt_table)

  # Extract times to speciation
  speciation_times <- sapply(1:(length(ltt_table$N) - 1), function(i) {
    ltt_table$time[i + 1] - ltt_table$time[i]
  })

  # Return table with corresponding values
  time_spec_table <- cbind(
    "N" = ltt_table$N[2:nrow(ltt_table)],
    speciation_times
    )
  tibble::as.tibble(time_spec_table)

}
