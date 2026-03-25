## This is the Lunn McNeil Model 


###################################################################

# Building the data frames :

###################################################################
# Building the dataset for A and B models
AB_data <- complete_cohort %>%
  mutate(
    er_Status = factor(er_Status, levels = c("Positive", "Negative", "Unknown"))
    )
#############################
# No lag time
positive <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_nolag == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_nolag == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_nolag == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
AB_lm_nolag <- rbind(positive, negative, unknown)
AB_lm_nolag <- AB_lm_nolag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
    )

#############################
# 2 lag time
positive <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_2lag == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_2lag == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_2lag == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
AB_lm_2lag <- rbind(positive, negative, unknown)
AB_lm_2lag <- AB_lm_2lag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )

#############################
# 5 lag time
positive <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_5lag == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_5lag == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- AB_data %>%
  mutate(
    bc_nolag = case_when(
      bc_5lag == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
AB_lm_5lag <- rbind(positive, negative, unknown)
AB_lm_5lag <- AB_lm_5lag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )

###################################################################

# Building for model C

C_data_nolag <- cohort_mensplit %>%
  mutate(
    er_Status = factor(er_Status, levels = c("Positive", "Negative", "Unknown"))
  )

C_data_2lag <- cohort_mensplit_2lag %>%
  mutate(
    er_Status = factor(er_Status, levels = c("Positive", "Negative", "Unknown"))
  )

C_data_5lag <- cohort_mensplit_5lag %>%
  mutate(
    er_Status = factor(er_Status, levels = c("Positive", "Negative", "Unknown"))
  )
#############################
# No lag time
positive <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
C_lm_nolag <- rbind(positive, negative, unknown)
C_lm_nolag <- C_lm_nolag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )

#############################
# 2 lag time
positive <- C_data_2lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- C_data_2lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- C_data_2lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
C_lm_2lag <- rbind(positive, negative, unknown)
C_lm_2lag <- C_lm_2lag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )

#############################
# 5 lag time
positive <- C_data_5lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Positive" ~ 1, 
      TRUE ~ 0
    ),
    type = "pos"
  )

negative <- C_data_5lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Negative" ~ 1, 
      TRUE ~ 0
    ),
    type = "neg"
  )

unknown <- C_data_5lag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & er_Status == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
C_lm_5lag <- rbind(positive, negative, unknown)
C_lm_5lag <- C_lm_5lag %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    pointNO2 = pointNO2/10,
    pointP25 = pointP25/5, 
    pointP10 = pointP10/5, 
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )


###################################################################

# Function to take out the HR, CI, and Wald values from the model
extract_lm_hr <- function(cox_fit, exposure_var, subtype_levels = c("pos", "neg", "unk")) {
  
  model_name <- deparse(substitute(cox_fit))
  
  # Extract coefficients and variance-covariance matrix
  coefs <- coef(cox_fit)
  vcov_mat <- vcov(cox_fit)
  
  # Check if main exposure exists
  if (!(exposure_var %in% names(coefs))) {
    stop(paste0("Exposure variable '", exposure_var, "' not found in model coefficients"))
  }
  
  # Main effect for reference subtype (first in subtype_levels)
  b_main <- coefs[exposure_var]
  
  # Find interaction terms
  interaction_terms <- grep(paste0("^", exposure_var, ":"), names(coefs), value = TRUE)
  
  # Initialize betas
  beta <- setNames(rep(0, length(subtype_levels)), subtype_levels)
  beta[subtype_levels[1]] <- b_main
  
  # Assign interaction betas
  for (term in interaction_terms) {
    for (subtype in subtype_levels[-1]) {
      if (grepl(subtype, term)) {
        beta[subtype] <- b_main + coefs[term]
      }
    }
  }
  
  # Calculate standard errors
  se <- setNames(rep(NA, length(subtype_levels)), subtype_levels)
  se[subtype_levels[1]] <- sqrt(vcov_mat[exposure_var, exposure_var])
  
  for (term in interaction_terms) {
    for (subtype in subtype_levels[-1]) {
      if (grepl(subtype, term)) {
        se[subtype] <- sqrt(
          vcov_mat[exposure_var, exposure_var] +
            vcov_mat[term, term] +
            2 * vcov_mat[exposure_var, term]
        )
      }
    }
  }
  
  # === Wald test for pos vs neg only ===
  wald_pval_pos_neg <- NA
  neg_term <- interaction_terms[grepl("neg", interaction_terms)]
  
  if (length(neg_term) == 1) {
    beta_diff <- coefs[neg_term]       # difference between pos and neg
    var_diff  <- vcov_mat[neg_term, neg_term]
    
    wald_stat <- (beta_diff^2) / var_diff
    wald_pval_pos_neg <- pchisq(wald_stat, df = 1, lower.tail = FALSE)
  }
  
  # Build results table
  result <- data.frame(
    Model = model_name,
    Subtype = subtype_levels,
    HR = exp(beta),
    CI_lower = exp(beta - 1.96 * se),
    CI_upper = exp(beta + 1.96 * se),
    Pval = 2 * pnorm(abs(beta / se), lower.tail = FALSE),
    Wald_pval = rep(wald_pval_pos_neg, length(subtype_levels))
  )
  
  result_clean <- result %>%
    mutate(
      HR = round(HR, 2),
      CI_lower = round(CI_lower, 2),
      CI_upper = round(CI_upper, 2),
      Pval = round(Pval, 3),
      Wald_pval = round(Wald_pval, 3)
    )
  
  return(result_clean)
}