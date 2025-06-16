# Simulate multiple trials using the simulate_single function

simulate_trials <- function(runs,
                            n_per_group,
                            p_control,
                            p_treatment,
                            return_list = FALSE,
                            ...) {
  sims <- vector("list", runs)
  for (i in seq_len(runs)) {
    sims[[i]] <- simulate_single(n_per_group,
                                 p_control,
                                 p_treatment,
                                 ...)
    sims[[i]]$run <- i
  }
  
  if (return_list) {
    return(sims)
  } else {
# stack into one big data.frame
    return(do.call(rbind, sims))
  }
}

