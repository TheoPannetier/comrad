fitness_func_ricker <- function(growth_rate,
                                 n_eff,
                                 carr_cap)
{
  # Compute fitness with the function from the Ricker model
  fitness <- exp(growth_rate * (1 - n_eff / carr_cap))

  # Solve possible NaN issues --------------------------------------------------
  if (
    # In case of conflict between parameters
    (growth_rate == 0 && any((n_eff / carr_cap) %in% c(Inf, -Inf))) ||
    (growth_rate == Inf && any((n_eff / carr_cap) == 1))
  ) {
    # I rule that growth_rate has precedence
    nans <- which(is.nan(fitness))
    fitness[nans] <- exp(growth_rate)
  }

  fitness
}
