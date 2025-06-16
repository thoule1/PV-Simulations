library(ggplot2)

# Create the parameters for the diagnostic runs
runs <- 500
n_per_group <- 800
p_control <- 0.35
p_treatment <- 0.27
rho <- -0.14

# 2) run sims and collect summaries
summ <- data.frame(
  run      = seq_len(runs),
  obs_ctrl = NA_real_,
  obs_trt  = NA_real_,
  corr_cov = NA_real_ )

for(i in summ$run) {
  sim <- simulate_single(
    n_per_group = n_per_group,
    p_control   = p_control,
    p_treatment = p_treatment,
    rho         = rho
  )
  summ$obs_ctrl[i] <- mean(sim$outcome[sim$group=="Control"])
  summ$obs_trt[i]  <- mean(sim$outcome[sim$group=="Treatment"])
  # ← compute cor on matrix-like input, then pull [1,2]
  summ$corr_cov[i] <- cor(sim[, c("SOFA","CRS_PBW")])[1,2]
}

# 3) histograms of observed event‐rates vs. targets
p_ctrl <- ggplot(summ, aes(obs_ctrl)) +
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "white") +
  geom_vline(xintercept = p_control, color = "red", linetype = "dashed") +
  labs(title = "Control Arm: observed event‐rate", x = "Observed rate", y = "Count")

p_trt <- ggplot(summ, aes(obs_trt)) +
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "white") +
  geom_vline(xintercept = p_treatment, color = "red", linetype = "dashed") +
  labs(title = "Treatment Arm: observed event‐rate", x = "Observed rate", y = "Count")

# 4) histogram of sample covariate correlation
p_rho <- ggplot(summ, aes(corr_cov)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  geom_vline(xintercept = rho, color = "red", linetype = "dashed") +
  labs(title = "Sample Correlation of (cov1,cov2)", x = "Cor(cov1,cov2)", y = "Count")

# 5) scatterplot of covariates in a single run
sim1 <- simulate_single(n_per_group, p_control, p_treatment, rho)
p_scatter <- ggplot(sim1, aes(SOFA, CRS_PBW)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Covariate Scatter (one run)", x = "SOFA", y = "CRS_PBW")

# Place all ggplots on one panel
library(gridExtra)
grid.arrange(p_ctrl, p_trt, p_rho, p_scatter, ncol = 2)


