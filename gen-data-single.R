library(MASS)

simulate_single <- function(n_per_group,
                            p_control,
                            p_treatment,
                            rho = -0.14,
                            b1  = log(1.11),
                            b2  = log(2.30),
                            calib_N = 20000) {
# total N for the actual trial
  N <- 2 * n_per_group
  group <- rep(c(0,1), each = n_per_group)
  
# simulate a large calibration sample to estimate expectations
  Xcal <- mvrnorm(calib_N, mu = c(0,0),
                  Sigma = matrix(c(1, rho, rho, 1), 2))
  grp_cal <- rep(c(0,1), each = calib_N/2)
  
# inner solver: given b_trt, find b0 so control arm = p_control
  solve_b0 <- function(b_trt, X, grp) {
    f0 <- function(b0) {
      lp0 <- b0 + b_trt * grp + b1 * X[,1] + b2 * X[,2]
      mean(plogis(lp0)[grp==0]) - p_control
    }
    uniroot(f0, c(-20,20))$root
  }
  
# outer solver: find b_trt so treatment arm = p_treatment
  f_trt <- function(b_trt) {
    b0_cal <- solve_b0(b_trt, Xcal, grp_cal)
    lp1    <- b0_cal + b_trt * grp_cal + b1 * Xcal[,1] + b2 * Xcal[,2]
    mean(plogis(lp1)[grp_cal==1]) - p_treatment
  }
  b_trt_hat <- uniroot(f_trt, c(-2, 2))$root
  
# with the calibrated b_trt, find the final b0
  b0_hat <- solve_b0(b_trt_hat, Xcal, grp_cal)
  
# simulate a trial dataset
  Xsim <- mvrnorm(N, mu = c(0,0),
                  Sigma = matrix(c(1, rho, rho, 1), 2))
  lp   <- b0_hat + b_trt_hat * group + b1 * Xsim[,1] + b2 * Xsim[,2]
  y    <- rbinom(N, 1, plogis(lp))
  
  data.frame(
    group   = factor(group, labels = c("Control","Treatment")),
    SOFA    = Xsim[,1],
    CRS_PBW    = Xsim[,2],
    outcome = y
  )
}
