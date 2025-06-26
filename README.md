# Bayesian Simulation for Clinical Trial Design
This repository contains R code and supporting documentation for a Bayesian simulation study designed to inform clinical trial design. The simulation evaluates a binary outcome (e.g., mortality) under a parallel arm design (e.g., treatment vs. control), with two prognostic covariates, under two alternative prior specifications.

## Weakly Informative Priors
These priors are centered at zero with relatively large standard deviations. They allow the data to primarily drive the inference while providing modest regularization to stabilize estimation. These priors can be user specified, but the defaults for the simulation are:

- Intercept ~ N(0,5)
- Covariate1 ~ N(0,2.5)
- Covariate2 ~ N(0,2.5)
- Treatment ~ N(0,2.5)

## Informative Priors
These priors incorporate external knowledge or previous research findings to influence posterior inference. They are more concentrated around expected effect sizes, reflecting greater certainty about plausible parameter values. Based on previous work for the trial being planned, the defaults for the simulation are:

- Intercept ~ N(-0.62,.5)
- Covariate1 ~ N(0.10,.25)
- Covariate2 ~ N(0.83,.25)
- Treatment ~ N(-0.38,.4)

## Simulation Overview
The simulation proceeds by:

- Generating synthetic datasets under a known data-generating process
  - gen-data-single.R (function to generate one run under a specific dgp)
  - gen-sim-multiple.R (wraps gen-data-single.R to generate multiple runs)
  - gen-data-single-dx.R (visually examines the generated data in comparison to desired values) 

- Conduct the simulations and analyzing using a bayesian model from brms 
  - run-sim-single.R (conducts many runs of the model)
  - visualize-sim.R (tabulates and visualizes the results of the simulation)

The entire process, above, may be executed using only execute-sim.R (weakly informative priors) or execute-sim-informative.R (informative priors)

## Outputs
The simulations output:

- Pr(Treatment Effect > 0% ARR) > 0.95
- Mean posterior effect estimate
- Mean CrI width
- Mean Pr(Effect > 0% ARR)
- Mean Pr(Effect > 2.5% ARR)






