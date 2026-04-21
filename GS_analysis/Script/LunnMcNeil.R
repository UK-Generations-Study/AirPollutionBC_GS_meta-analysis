## This is the Lunn McNeil Model 


###################################################################

# Building the data frames :

###################################################################

# Building for model C

C_data_nolag <- cohort_mensplit %>%
  mutate(
    er_Status = factor(event_er,
                levels = c(0, 1, 2, 3),
                labels = c("Nonevent", "Positive", "Negative", "Unknown")),
    
    screen = factor(screen_detected,
                    levels = c(0, 1, 2, 3),
                    labels = c("Nonevent", "Yes", "No", "Unknown")),
    
    invdcis = factor(inv_dcis,
                     levels = c(0, 1, 2, 3),
                     labels = c("Nonevent", "Invasive", "DCIS", "Unknown"))
  )

#############################
# ER status
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
er_status <- rbind(positive, negative, unknown)
er_status <- er_status %>%
  mutate(
    type = factor(type, levels = c("pos", "neg", "unk")),
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )


# Screen detected status
yesscreen <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Yes" ~ 1, 
      TRUE ~ 0
    ),
    type = "screen"
  )

noscreen <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "No" ~ 1, 
      TRUE ~ 0
    ),
    type = "noscreen"
  )


unknown <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
screened <- rbind(yesscreen, noscreen, unknown)
screened <- screened %>%
  mutate(
    type = factor(type, levels = c("screen", "noscreen", "unk")),
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )


# DCIS vs Invasive
invasive <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & invdcis == "Invasive" ~ 1, 
      TRUE ~ 0
    ),
    type = "invasive"
  )

dcis <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & invdcis == "DCIS" ~ 1, 
      TRUE ~ 0
    ),
    type = "dcis"
  )

unknown <- C_data_nolag %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & invdcis == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
invdcis_data <- rbind(invasive, dcis, unknown)
invdcis_data <- invdcis_data %>%
  mutate(
    type = factor(type, levels = c("invasive", "dcis", "unk")),
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )
###################################################################

# Function to take out the HR, CI, and Wald values from the model
extract_lm_hr <- function(cox_fit, exposure_var, subtype_levels = c("pos", "neg", "unk")) {
  
  model_name <- deparse(substitute(cox_fit))
  
  coefs <- coef(cox_fit)
  vcov_mat <- vcov(cox_fit)
  
  if (!(exposure_var %in% names(coefs))) {
    stop(paste0("Exposure variable '", exposure_var, "' not found in model coefficients"))
  }
  
  b_main <- coefs[exposure_var]
  
  interaction_terms <- grep(paste0("^", exposure_var, ":"), names(coefs), value = TRUE)
  
  beta <- setNames(rep(0, length(subtype_levels)), subtype_levels)
  beta[subtype_levels[1]] <- b_main
  
  for (term in interaction_terms) {
    for (subtype in subtype_levels[-1]) {
      if (grepl(subtype, term)) {
        beta[subtype] <- b_main + coefs[term]
      }
    }
  }
  
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
  
  # === Wald test: subtype_levels[1] vs subtype_levels[2] only ===
  wald_pval <- NA
  subtype2_term <- interaction_terms[grepl(subtype_levels[2], interaction_terms)]  
  
  if (length(subtype2_term) == 1) {
    beta_diff <- coefs[subtype2_term]
    var_diff  <- vcov_mat[subtype2_term, subtype2_term]
    
    wald_stat <- (beta_diff^2) / var_diff
    wald_pval <- pchisq(wald_stat, df = 1, lower.tail = FALSE)
  }
  
  result <- data.frame(
    Model = model_name,
    Subtype = subtype_levels,
    HR = exp(beta),
    CI_lower = exp(beta - 1.96 * se),
    CI_upper = exp(beta + 1.96 * se),
    Pval = 2 * pnorm(abs(beta / se), lower.tail = FALSE),
    Wald_pval = rep(wald_pval, length(subtype_levels))
  )
  
  result %>%
    mutate(
      HR = round(HR, 2),
      CI_lower = round(CI_lower, 2),
      CI_upper = round(CI_upper, 2),
      Pval = round(Pval, 3),
      Wald_pval = round(Wald_pval, 3)
    )
  
  return(result)
}