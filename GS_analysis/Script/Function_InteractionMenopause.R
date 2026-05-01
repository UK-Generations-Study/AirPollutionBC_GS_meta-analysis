# To be called in "TimeSplit_Menopause.R"

# This is a function that takes  a cox model with a menopause interaction term and returns the HR of both groups 
# Example: 
# Original model: 
# C_P10_int <- coxph(Surv(tstart, tstop, bc_5lag) ~ I(avgP10/5) * menobandf + matchyear_cat + BMI_cat + IMD_cat + Smoking_cat + Alcohol_cat + Menarche_cat + Parity_cat + Birth1_age_cat +HRT_cat, 
# Call the function: 
# extract_hr_ci(C_P10_int, "C_P10_int")


extract_hr_ci <- function(model, model_name) {
  # Parse model_name like "A_NO2_int"
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'A_NO2_int')")
  }
  
  model_id <- name_parts[1]
  pollutant <- name_parts[2]  # e.g., "NO2"
  term_type <- name_parts[3]
  
  pollutant_var <- paste0("point", pollutant)  # e.g., "pointNO2"
  interaction_label <- "menobandfpostmenopausal"
  
  coef_df <- summary(model)$coefficients
  vcov_mat <- vcov(model)
  
  # Identify pollutant main effect and interaction term
  pollutant_term <- rownames(coef_df)[rownames(coef_df) == pollutant_var]
  interaction_term <- rownames(coef_df)[rownames(coef_df) == paste0(pollutant_var, ":", interaction_label)]
  
  if (length(pollutant_term) != 1 || length(interaction_term) != 1) {
    stop(paste("Could not uniquely identify terms in model:", model_name))
  }
  
  # Extract coefficients and SEs
  logHR_pre <- coef_df[pollutant_term, "coef"]
  se_pre <- coef_df[pollutant_term, "se(coef)"]
  p_pre <- coef_df[pollutant_term, "Pr(>|z|)"]  # p-value pre
  
  logHR_post <- logHR_pre + coef_df[interaction_term, "coef"]
  se_post <- sqrt(
    se_pre^2 +
      coef_df[interaction_term, "se(coef)"]^2 +
      2 * vcov_mat[pollutant_term, interaction_term]
  )
  # Calculate z and p-value post
  z_post <- logHR_post / se_post
  p_post <- 2 * (1 - pnorm(abs(z_post)))
  
  # Exponentiate to get HRs and 95% CIs
  HR_pre <- exp(logHR_pre)
  CI_lower_pre <- exp(logHR_pre - 1.96 * se_pre)
  CI_upper_pre <- exp(logHR_pre + 1.96 * se_pre)
  
  HR_post <- exp(logHR_post)
  CI_lower_post <- exp(logHR_post - 1.96 * se_post)
  CI_upper_post <- exp(logHR_post + 1.96 * se_post)
  
  # Return results with p-values rounded to 4 decimals
  data.frame(
    Model = model_id,
    pollutant = pollutant,
    term = term_type,
    HR_pre = round(HR_pre, 2),
    CI_lower_pre = round(CI_lower_pre, 2), 
    CI_upper_pre = round(CI_upper_pre, 2), 
    pvalue_pre = signif(p_pre, 4),
    HR_post = round(HR_post, 2), 
    CI_lower_post = round(CI_lower_post, 2), 
    CI_upper_post = round(CI_upper_post, 2),
    pvalue_post = signif(p_post, 4)
  )
}

extract_hr_ci_avg <- function(model, model_name) {
  # Parse model_name like "A_NO2_int"
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'A_NO2_int')")
  }
  
  model_id <- name_parts[1]
  pollutant <- name_parts[2]  # e.g., "NO2"
  term_type <- name_parts[3]
  
  pollutant_var <- paste0("avg", pollutant)  # e.g., "avgNO2"
  interaction_label <- "menobandfpostmenopausal"
  
  coef_df <- summary(model)$coefficients
  vcov_mat <- vcov(model)
  
  # Identify pollutant main effect and interaction term
  pollutant_term <- rownames(coef_df)[rownames(coef_df) == pollutant_var]
  interaction_term <- rownames(coef_df)[rownames(coef_df) == paste0(pollutant_var, ":", interaction_label)]
  
  if (length(pollutant_term) != 1 || length(interaction_term) != 1) {
    stop(paste("Could not uniquely identify terms in model:", model_name))
  }
  
  # Extract coefficients and SEs
  logHR_pre <- coef_df[pollutant_term, "coef"]
  se_pre <- coef_df[pollutant_term, "se(coef)"]
  p_pre <- coef_df[pollutant_term, "Pr(>|z|)"]  # p-value pre
  
  logHR_post <- logHR_pre + coef_df[interaction_term, "coef"]
  se_post <- sqrt(
    se_pre^2 +
      coef_df[interaction_term, "se(coef)"]^2 +
      2 * vcov_mat[pollutant_term, interaction_term]
  )
  # Calculate z and p-value post
  z_post <- logHR_post / se_post
  p_post <- 2 * (1 - pnorm(abs(z_post)))
  
  # Exponentiate to get HRs and 95% CIs
  HR_pre <- exp(logHR_pre)
  CI_lower_pre <- exp(logHR_pre - 1.96 * se_pre)
  CI_upper_pre <- exp(logHR_pre + 1.96 * se_pre)
  
  HR_post <- exp(logHR_post)
  CI_lower_post <- exp(logHR_post - 1.96 * se_post)
  CI_upper_post <- exp(logHR_post + 1.96 * se_post)
  
  # Return results with p-values
  data.frame(
    Model = model_id,
    pollutant = pollutant,
    term = term_type,
    HR_pre = HR_pre,
    CI_lower_pre = CI_lower_pre,
    CI_upper_pre = CI_upper_pre,
    pvalue_pre = p_pre,
    HR_post = HR_post,
    CI_lower_post = CI_lower_post,
    CI_upper_post = CI_upper_post,
    pvalue_post = p_post
  )
}


extract_hr_ci_carstairs <- function(model, model_name) {
  
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'A_P25_int')")
  }
  
  model_id  <- name_parts[1]
  pollutant <- name_parts[2]
  term_type <- name_parts[3]
  
  pollutant_var <- paste0("avg", pollutant)
  
  coef_df  <- summary(model)$coefficients
  vcov_mat <- vcov(model)
  
  # Verify main pollutant term exists
  pollutant_term <- rownames(coef_df)[rownames(coef_df) == pollutant_var]
  if (length(pollutant_term) != 1) {
    stop(paste("Could not uniquely identify pollutant term in model:", model_name))
  }
  
  # --- Dynamically detect interaction terms ---
  # Works for both "avgP25:type*" (LM stacked) and "avgP25:Carstairs_quint*" (standard)
  all_terms      <- rownames(coef_df)
  int_pattern    <- paste0("^", pollutant_var, ":")
  int_term_names <- all_terms[grepl(int_pattern, all_terms)]
  
  if (length(int_term_names) == 0) {
    stop(paste("No interaction terms found matching pattern:", int_pattern))
  }
  
  # Extract level labels by stripping everything up to and including ":"
  level_names <- sub(paste0("^", pollutant_var, ":Carstairs_quint"), "", int_term_names)
  names(int_term_names) <- level_names
  
  # --- Reference level (Carstairs quintile 1 = most deprived) ---
  logHR_ref <- coef_df[pollutant_term, "coef"]
  se_ref    <- coef_df[pollutant_term, "se(coef)"]
  p_ref     <- coef_df[pollutant_term, "Pr(>|z|)"]
  
  # --- Non-reference levels via delta method ---
  extract_non_ref <- function(level) {
    int_term <- int_term_names[level]
    logHR    <- logHR_ref + coef_df[int_term, "coef"]
    se       <- sqrt(
      se_ref^2 +
        coef_df[int_term, "se(coef)"]^2 +
        2 * vcov_mat[pollutant_term, int_term]
    )
    z    <- logHR / se
    pval <- 2 * (1 - pnorm(abs(z)))
    
    data.frame(logHR = logHR, se = se, pval = pval)
  }
  
  non_ref <- lapply(level_names, extract_non_ref)
  names(non_ref) <- level_names
  
  # --- Reference row ---
  ref_row <- data.frame(
    Model     = model_id,
    pollutant = pollutant,
    term      = term_type,
    Carstairs = "1 (most)",
    HR        = exp(logHR_ref),
    CI_lower  = exp(logHR_ref - 1.96 * se_ref),
    CI_upper  = exp(logHR_ref + 1.96 * se_ref),
    pvalue    = p_ref
  )
  
  # --- Non-reference rows ---
  # Map raw level names to clean labels for output
  label_map <- c(
    "2"          = "2",
    "3"          = "3",
    "4"          = "4",
    "5 (least)"  = "5 (least)",
    "Unknown"    = "Unknown"
  )
  
  non_ref_rows <- bind_rows(lapply(level_names, function(level) {
    data.frame(
      Model     = model_id,
      pollutant = pollutant,
      term      = term_type,
      Carstairs = ifelse(level %in% names(label_map), label_map[level], level),
      HR        = exp(non_ref[[level]]$logHR),
      CI_lower  = exp(non_ref[[level]]$logHR - 1.96 * non_ref[[level]]$se),
      CI_upper  = exp(non_ref[[level]]$logHR + 1.96 * non_ref[[level]]$se),
      pvalue    = non_ref[[level]]$pval
    )
  }))
  
  result <- bind_rows(ref_row, non_ref_rows) %>%
    mutate(
      HR       = round(HR, 2),
      CI_lower = round(CI_lower, 2),
      CI_upper = round(CI_upper, 2),
      pvalue   = round(pvalue, 3)
    )
  
  return(result)
}

# This function pulls the pvalue from a LRT test
# Example: 
# Original model: 
# C_P10_anova <- anova(C_P10_base, C_P10_int, test = "LRT")

# Call the function: 
# extract_lrt_pval(C_P10_anova, "C_P10_anova")

extract_lrt_pval <- function(lrt_result, model_name) {
  # Parse model name into components
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'C_P10_int')")
  }
  model_id <- name_parts[1]
  pollutant <- name_parts[2]
  term_type <- name_parts[3]
  
  # Check if the correct column exists
  if (!("Pr(>|Chi|)" %in% colnames(lrt_result))) {
    warning(paste("LRT result does not contain expected column 'Pr(>|Chi|)' for:", model_name))
    pval <- NA
  } else if (nrow(lrt_result) < 2) {
    warning(paste("LRT result does not contain two model rows for:", model_name))
    pval <- NA
  } else {
    # Extract p-value from second row
    pval <- lrt_result[2, "Pr(>|Chi|)"]
  }
  
  # Return as single-row data frame
  data.frame(
    Model = model_id,
    pollutant = pollutant,
    term = term_type,
    LRT_pval = pval
  )
}

