source("gen-data-single.R")
source("gen-sim-multiple.R")

# ── 0. prerequisites ─────────────────────────────────────────────────────
library(brms)
library(tibble)
library(tidyverse)

# ── 1. user‐specified parameters ─────────────────────────────────────────
runs         <- 100
n_per_group  <- 400
p_control    <- 0.35
p_treatment  <- 0.27

# prior hyper-parameters: each = c(mean, sd)
intercept_prior <- c(0, 5)
SOFA_prior      <- c(0, 2.5)
CRS_PBW_prior   <- c(0, 2.5)
trt_prior       <- c(0, 2.5)

# OR thresholds and posterior-prob cutoffs
or_thresholds   <- c(1, 0.8)
prob_thresholds <- c(0.8, 0.9, 0.95)

# MCMC settings
iter   <- 2000
warmup <- 500
chains <- 4
seed   <- 76

# ── 2. build brms prior list ─────────────────────────────────────────────
priors <- c(
  set_prior(paste0("normal(", intercept_prior[1], ", ", intercept_prior[2], ")"), class = "Intercept"),
  set_prior(paste0("normal(", SOFA_prior[1], ", ", SOFA_prior[2], ")"), class = "b", coef = "SOFA"),
  set_prior(paste0("normal(", CRS_PBW_prior[1], ", ", CRS_PBW_prior[2], ")"), class = "b", coef = "CRS_PBW"),
  set_prior(paste0("normal(", trt_prior[1], ", ", trt_prior[2], ")"), class = "b", coef = "groupTreatment")
)


# ── 3. generate list of simulated datasets ───────────────────────────────
#    assume simulate_trials() is in your workspace and supports return_list=TRUE
sim_list <- simulate_trials(
  runs        = runs,
  n_per_group = n_per_group,
  p_control   = p_control,
  p_treatment = p_treatment,
  return_list = TRUE
)

compiled_fit <- brm(
  formula = outcome ~ SOFA + CRS_PBW + group,
  data    = sim_list[[1]],
  family  = bernoulli(),
  prior   = priors,
  iter    = 2,           # minimal valid sampling
  warmup  = 1,
  chains  = 1,
  seed    = 999,
  refresh = 0
)

# Save the compiled model
saveRDS(compiled_fit, file = "cached_model.rds")

# ── 4. analyze each dataset with brms ────────────────────────────────────
# Initialize empty list to store results
results_list <- vector("list", length = runs)
compiled_fit <- readRDS("cached_model.rds") 

for (i in seq_len(runs)) {
  dat <- sim_list[[i]]
  
  # Fit the Bayesian logistic regression
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
  
  # Extract posterior draws
  post <- as_draws_df(fit)
  
  # Key posterior summaries
  p_lt_0     <- mean(post$b_groupTreatment < 0)
  p_lt_0.2   <- mean(post$b_groupTreatment < -0.2)
  p_lt_0.5   <- mean(post$b_groupTreatment < -0.5)
  p_gt_0.1 <- mean(post$b_groupTreatment > 0.1)
  
  est <- posterior_summary(fit, pars = "b_groupTreatment")
  mean_effect <- est[, "Estimate"]
  lower_95    <- est[, "Q2.5"]
  upper_95    <- est[, "Q97.5"]
  
  # Binary decision rule: Pr(β < 0) > 0.95
  decision <- p_lt_0 > 0.95
  
  # Store results
  results_list[[i]] <- tibble(
    sim = i,
    mean_effect = mean_effect,
    lower_95 = lower_95,
    upper_95 = upper_95,
    p_lt_0 = p_lt_0,
    p_lt_0.2 = p_lt_0.2,
    p_t_0.5 = p_lt_0.5,
    p_gt_0.1 = p_gt_0.1,
    decision = decision
  )
}

# Combine all simulation results into one data frame
all_results <- bind_rows(results_list)

