# Quantifying bias

# Carstairs Index

# Air pollution modelling 
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

measurementerror_quantification <- function(beta, se) {

  set.seed(123)
  n    <- 100000
  
  # ── Parameter distributions ───────────────────────────────────────────────────
  # Lambda: classical error attenuation (Shen et al. lamda comparison with, range 0.69–0.82)
  # Modelled as a Beta scaled to [0.69, 0.82]:
  #   mean ≈ 0.755, tight spread consistent with reported range
  lambda_draws <- 0.69 + rbeta(n, shape1 = 5, shape2 = 5) * (0.82 - 0.69)


  # ── Monte Carlo bias-adjusted betas ──────────────────────────────────────────
  beta_classical <- beta  / lambda_draws          # point estimate corrected
  
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

