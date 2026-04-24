# Quantifying bias

# Carstairs Index

# Air pollution modelling 
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

measurementerror_quantification <- function(beta, se) {
  lambdas <- c(0.69, 0.75, 0.82)
  slopes  <- c(0.39, 0.56, 0.73)
  
  set.seed(123)
  n    <- 100000
  
  # ── Parameter distributions ───────────────────────────────────────────────────
  # Lambda: classical error attenuation (Shen et al. CV-R², range 0.69–0.82)
  # Modelled as a Beta scaled to [0.69, 0.82]:
  #   mean ≈ 0.755, tight spread consistent with reported range
  lambda_draws <- 0.69 + rbeta(n, shape1 = 5, shape2 = 5) * (0.82 - 0.69)
  
  # Slope: Berkson personal-to-ambient ratio (meta-analysis mean 0.56, 95% CI 0.39–0.73)
  # Modelled as Normal truncated at (0, 1):
  #   mean = 0.56, SD derived so ±1.96 SD ≈ 0.39–0.73 → SD ≈ 0.0867
  slope_raw    <- rnorm(n, mean = 0.56, sd = 0.0867)
  slope_draws  <- pmax(0.01, pmin(1, slope_raw))   # truncate to (0, 1]
  
  # ── Monte Carlo bias-adjusted betas ──────────────────────────────────────────
  beta_classical <- beta  / lambda_draws          # point estimate corrected
  beta_combined  <- beta  / lambda_draws          # same correction for combined
  
  se_berkson     <- se    / slope_draws           # SE inflated for Berkson
  se_combined    <- se    / slope_draws           # SE inflated for combined
  
  # ── Simulation draws of HR (with parameter uncertainty) ──────────────────────
  # For each draw, sample a single HR from N(beta_adj, se_adj)
  hr_classical <- exp(rnorm(n, beta_classical, se / lambda_draws))
  
  # ── Summarise into a results dataframe ───────────────────────────────────────
  summarise_draws <- function(draws, label) {
    tibble(
      `Bias Adjustment`    = label,
      `HR`     = round(median(draws), 4),
      `Lower CI`   = round(quantile(draws, 0.025), 4),
      `Upper CI`  = round(quantile(draws, 0.975), 4)
    )
  }
  
  biasresults_probablistic <- summarise_draws(hr_classical, "EXPANSE Model Accuracy")
  
  return(biasresults_probablistic)
  
}

