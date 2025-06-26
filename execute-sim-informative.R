library(brms)
library(tibble)
library(tidyverse)
library(patchwork)

# Simulation and summary functions
source("run-sim-single.R")
source("visualize-sim.R")
source("gen-data-single.R")
source("gen-sim-multiple.R")

# Define simulation parameters
runs         <- 500
n_per_group  <- 400
p_control    <- 0.35
p_treatment  <- 0.27

# Run simulation and model fitting
results <- run_brms_simulations(
  runs = runs,
  n_per_group = n_per_group,
  p_control = p_control,
  p_treatment = p_treatment,
  intercept_prior = c(-.62, .5),
  SOFA_prior = c(.10, .25),
  CRS_PBW_prior = c(.83, .25),
  trt_prior = c(-.38, .4),
  iter = 2000,
  warmup = 500,
  chains = 4,
  seed = 782,
  model_cache_path = "cached_model.rds"
)

# Create the plot
final_plot <- summarize_simulation_results(
  all_results  = results,
  n_per_group  = n_per_group,
  p_control    = p_control,
  p_treatment  = p_treatment
)

# Display the plot
print(final_plot)

# Optional: Save the plot
ggsave("n400_informed.png", final_plot, width = 12, height = 8, dpi = 300)
