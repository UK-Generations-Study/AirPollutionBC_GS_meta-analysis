## PAF for PM with Monte Carlo uncertainty
## Assumptions: log-linear RR, exposure X ~ Normal(mu, sigma^2)
set.seed(20251027)

###***PM2.5***###
mu_PM2.5 <- 9.577           # population mean of PM2.5
sigma_PM2.5 <- 1.389          # population SD of PM2.5
xref_PM2.5 <- 5            # counterfactual PM2.5 (WHO AQG)
RR10_PM2.5 <- 1.06
CI10_PM2.5 <- c(1.03, 1.09)

## ---- Derive SE on log scale from CI ----
logRR10_PM2.5 <- log(RR10_PM2.5)
SE_logRR10_PM2.5  <- (log(CI10_PM2.5[2]) - log(CI10_PM2.5[1])) / (2 * 1.96)

## ---- Monte Carlo settings ----
B <- 10000  # number of draws

## ---- Draw effect (uncertainty from meta-analytic RR) ----
logRR10_draws_PM2.5 <- rnorm(B, mean = logRR10_PM2.5, sd = SE_logRR10_PM2.5)
beta_draws_PM2.5 <- logRR10_draws_PM2.5 / 10   # per 1 ug/m^3 slope

## ---- (A) Distribution-integrated PAF (recommended) ----
## E[RR(X)] = E[exp{beta*(X - xref)}] for X ~ N(mu, sigma^2)
## = exp( beta*(mu - xref) + 0.5 * beta^2 * sigma^2 )
E_RR_draws_PM2.5 <- exp(beta_draws_PM2.5 * (mu_PM2.5 - xref_PM2.5) + 0.5 * (beta_draws_PM2.5^2) * (sigma_PM2.5^2))
PAF_draws_PM2.5  <- (E_RR_draws_PM2.5 - 1) / E_RR_draws_PM2.5

PAF_point_PM2.5  <- mean(PAF_draws_PM2.5)  # Monte Carlo mean as point estimate
PAF_CI_PM2.5  <- quantile(PAF_draws_PM2.5, c(0.025, 0.975))

## ---- (B) Mean-only approximation (sensitivity) ----
RRpop_meanonly_draws_PM2.5 <- exp(beta_draws_PM2.5 * (mu_PM2.5 - xref_PM2.5))
PAF_meanonly_draws_PM2.5 <- (RRpop_meanonly_draws_PM2.5 - 1) / RRpop_meanonly_draws_PM2.5
PAF_meanonly_point_PM2.5 <- mean(PAF_meanonly_draws_PM2.5)
PAF_meanonly_CI_PM2.5 <- quantile(PAF_meanonly_draws_PM2.5, c(0.025, 0.975))

## ---- Print tidy results ----
as_percent <- function(x) sprintf('%.1f%%', 100 * x)

cat("=== Distribution-integrated PAF (recommended) ===\n")
cat("Point estimate: ", as_percent(PAF_point_PM2.5), "\n", sep = "")
cat("95% UI:         [", as_percent(PAF_CI_PM2.5[1]), ", ", as_percent(PAF_CI_PM2.5[2]), "]\n\n", sep = "")

cat("=== Mean-only approximation (sensitivity) ===\n")
cat("Point estimate: ", as_percent(PAF_meanonly_point_PM2.5), "\n", sep = "")
cat("95% UI:         [", as_percent(PAF_meanonly_CI_PM2.5[1]), ", ", as_percent(PAF_meanonly_CI_PM2.5[2]), "]\n\n", sep = "")
