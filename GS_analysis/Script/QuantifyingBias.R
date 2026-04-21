# Quantifying bias

# Carstairs Index

# Air pollution modelling 
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

measurementerror_quantification <- function(beta, se) {
  lambdas <- c(0.69, 0.75, 0.82)
  slopes  <- c(0.39, 0.56, 0.73)
  
  # ── 1. Naive ─────────────────────────────────────────────────────────────────
  naive <- tibble(
    adjustment  = "Original Estimate",
    lambda      = NA_real_,
    slope       = NA_real_,
    hr          = exp(beta),
    ci_low      = exp(beta - 1.96 * se),
    ci_high     = exp(beta + 1.96 * se)
  )
  
  # ── 2. Classical error only ───────────────────────────────────────────────────
  classical <- map_dfr(lambdas, function(l) {
    bc  <- beta / l
    sec <- se   / l
    tibble(
      adjustment = "Bias Adjusted (Expanse Model vs Actual Pollution)",
      lambda     = l,
      slope      = NA_real_,
      hr         = exp(bc),
      ci_low     = exp(bc - 1.96 * sec),
      ci_high    = exp(bc + 1.96 * sec)
    )
  })
  
  # ── 3. Berkson error only ─────────────────────────────────────────────────────
  berkson <- map_dfr(slopes, function(s) {
    sec <- se / s
    tibble(
      adjustment = "Bias Adjusted (Ambient vs. Individual Pollution)",
      lambda     = NA_real_,
      slope      = s,
      hr         = exp(beta),
      ci_low     = exp(beta - 1.96 * sec),
      ci_high    = exp(beta + 1.96 * sec)
    )
  })
  
  # ── 4. Combined ───────────────────────────────────────────────────────────────
  combined <- cross_df(list(lambda = lambdas, slope = slopes)) |>
    mutate(
      adjustment = "Bias Adjusted (Combined)",
      bc         = beta / lambda,
      sec        = se   / slope,
      hr         = exp(bc),
      ci_low     = exp(bc - 1.96 * sec),
      ci_high    = exp(bc + 1.96 * sec)
    ) |>
    select(adjustment, lambda, slope, hr, ci_low, ci_high)
  
  # ── 5. Bind all results ───────────────────────────────────────────────────────
  biasresults_static <- bind_rows(naive, classical, berkson, combined) |>
    mutate(
      across(c(hr, ci_low, ci_high), \(x) round(x, 4)), 
      ci_low = round(ci_low, 2), 
      included = case_when(
        adjustment == "Bias Adjusted (Combined)" & lambda == 0.75 & slope == 0.56 ~1, 
        adjustment !="Bias Adjusted (Combined)" ~ 1, 
        TRUE ~ 0
      )
    ) %>%
    filter(included ==1) %>%
    select (-included)
  
  
  
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
  hr_berkson   <- exp(rnorm(n, beta,           se / slope_draws))
  hr_combined  <- exp(rnorm(n, beta_combined,  se_combined))
  
  # ── Summarise into a results dataframe ───────────────────────────────────────
  summarise_draws <- function(draws, label) {
    tibble(
      `Bias Adjustment`    = label,
      `HR`     = round(median(draws), 4),
      `Lower CI`   = round(quantile(draws, 0.025), 4),
      `Upper CI`  = round(quantile(draws, 0.975), 4)
    )
  }
  
  biasresults_probablistic <- bind_rows(
    tibble(`Bias Adjustment`  = "Original Estimate",
           `HR` = round(exp(beta), 4),
           `Lower CI` = round(exp(beta - 1.96 * se), 4),
           `Upper CI`= round(exp(beta + 1.96 * se), 4)),
    summarise_draws(hr_classical, "Probabilistic Bias Adjusted (Expanse Model vs Actual Pollution)"),
    summarise_draws(hr_berkson,   "Probabilistic Bias Adjusted (Ambient vs. Individual Pollution)"),
    summarise_draws(hr_combined,  "Probabilistic Bias Adjusted (Combined)")
  )
  
  
  # ── Visualise simulation distributions ───────────────────────────────────────
  sim_long <- tibble(
    `Expanse Model Bias` = hr_classical,
    `Ambient vs. Personal`   = hr_berkson,
    Combined  = hr_combined
  ) |>
    pivot_longer(everything(), names_to = "adjustment", values_to = "hr") |>
    filter(hr > 0.5, hr < 2)   # trim extreme tails for plotting
  
  biasresults_probablistic_graph <- ggplot(sim_long, aes(x = hr, fill = adjustment)) +
    geom_density(alpha = 0.4) +
    geom_vline(xintercept = exp(beta), linetype = "dashed", colour = "black") +
    geom_vline(xintercept = 1,         linetype = "dotted",  colour = "grey40") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Probabilistic bias analysis",
      subtitle = "Dashed = naive HR; dotted = null (HR 1.0)",
      x        = "Hazard ratio",
      y        = "Density",
      fill     = "Adjustment"
    ) +
    theme_minimal(base_size = 13)
  
  return(list(
    biasresults_static = biasresults_static,
    biasresults_probablistic = biasresults_probablistic,
    biasresults_probablistic_graph = biasresults_probablistic_graph
  ))
  
}

