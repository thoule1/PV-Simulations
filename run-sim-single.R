run_brms_simulations <- function(
    runs         = 200,
    n_per_group  = 400,
    p_control    = 0.35,
    p_treatment  = 0.27,
    intercept_prior = c(0, 5),
    SOFA_prior      = c(0, 2.5),
    CRS_PBW_prior   = c(0, 2.5),
    trt_prior       = c(0, 2.5),
    iter   = 2000,
    warmup = 500,
    chains = 4,
    seed   = 351,
    model_cache_path = "cached_model.rds"
) {
  library(brms)
  library(tibble)
  library(tidyverse)
  
# Load required data simulation functions
  source("gen-data-single.R")
  source("gen-sim-multiple.R")
  
# Create brms prior list
  priors <- c(
    set_prior(paste0("normal(", intercept_prior[1], ", ", intercept_prior[2], ")"), class = "Intercept"),
    set_prior(paste0("normal(", SOFA_prior[1], ", ", SOFA_prior[2], ")"), class = "b", coef = "SOFA"),
    set_prior(paste0("normal(", CRS_PBW_prior[1], ", ", CRS_PBW_prior[2], ")"), class = "b", coef = "CRS_PBW"),
    set_prior(paste0("normal(", trt_prior[1], ", ", trt_prior[2], ")"), class = "b", coef = "groupTreatment")
  )
  
# Generate simulated datasets
  sim_list <- simulate_trials(
    runs        = runs,
    n_per_group = n_per_group,
    p_control   = p_control,
    p_treatment = p_treatment,
    return_list = TRUE
  )
  
# Compile base model if not already saved
  if (!file.exists(model_cache_path)) {
    compiled_fit <- brm(
      formula = outcome ~ SOFA + CRS_PBW + group,
      data    = sim_list[[1]],
      family  = bernoulli(),
      prior   = priors,
      iter    = 2,
      warmup  = 1,
      chains  = 1,
      seed    = 999,
      refresh = 0
    )
    saveRDS(compiled_fit, file = model_cache_path)
  }
  
  compiled_fit <- readRDS(model_cache_path)
  results_list <- vector("list", length = runs)
  
  for (i in seq_len(runs)) {
    dat <- sim_list[[i]]
    print(i)
    fit <- update(
      object = compiled_fit,
      newdata = dat,
      seed = seed + i,
      iter = iter,
      warmup = warmup,
      chains = chains,
      refresh = 0,
      recompile = FALSE
    )
    
    post <- as_draws_df(fit)
    
# Save several posterior probabilities
    p_lt_0   <- mean(post$b_groupTreatment < 0)
    # Solve for a 2.5% ARR from the control group; convert to logit
    mcid <- log(p_control / (1 - p_control)) - log((p_control - .025 )  / (1 - p_control + .025))
    p_lt_0.2 <- mean(post$b_groupTreatment < -mcid)
    p_gt_0.1 <- mean(post$b_groupTreatment > 0.1)
    
    est <- posterior_summary(fit, variable = "b_groupTreatment")
    
    results_list[[i]] <- tibble(
      sim         = i,
      mean_effect = est[, "Estimate"],
      lower_95    = est[, "Q2.5"],
      upper_95    = est[, "Q97.5"],
      p_lt_0      = p_lt_0,
      p_lt_0.2    = p_lt_0.2,
      p_lt_0.5    = p_lt_0.5,
      p_gt_0.1    = p_gt_0.1,
      decision    = p_lt_0 > 0.95
    )
  }
  
  all_results <- bind_rows(results_list)
  return(all_results)
}
