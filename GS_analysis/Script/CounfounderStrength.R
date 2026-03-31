# Looking at strength of confounders
library(purrr)
library(effectsize)

# Categorical confounders of interest
cat_vars <- c("matchyear_cat", "BMI_cat", "Carstairs_cat", "Smoking_cat", "Alcohol_cat", "Educ_cat", "Parity_cat", "Birth1_age_cat", "HRT_cat")

# PM2.5
P25association_results <- map_dfr(cat_vars, function(var) {
  formula <- as.formula(paste("avgP25 ~", var))
  fit <- aov(formula, data = complete_cohort)
  
  # Effect size
  eta <- eta_squared(fit)
  
  data.frame(
    variable = var,
    eta_squared = eta$Eta2,
    p_value = summary(fit)[[1]][["Pr(>F)"]][1]  # ANOVA p-value
  )
})

P25_confounder_results <- P25association_results %>%
  mutate(
    PM25_eta_squared = round(eta_squared, 4), 
   # PM25_p_value = round(p_value, 5)
  ) %>%
  select(
    -eta_squared, -p_value
  )


# PM 10
P10association_results <- map_dfr(cat_vars, function(var) {
  formula <- as.formula(paste("avgP10 ~", var))
  fit <- aov(formula, data = complete_cohort)
  
  # Effect size
  eta <- eta_squared(fit)
  
  data.frame(
    variable = var,
    eta_squared = eta$Eta2,
    p_value = summary(fit)[[1]][["Pr(>F)"]][1]  # ANOVA p-value
  )
})

P10_confounder_results <- P10association_results %>%
  mutate(
    PM10_eta_squared = round(eta_squared, 4), 
   # PM10_p_value = round(p_value, 5)
  )%>%
  select(
    -eta_squared, -p_value
  )




all_anova <- P25_confounder_results %>%
  left_join(P10_confounder_results, by = "variable") 