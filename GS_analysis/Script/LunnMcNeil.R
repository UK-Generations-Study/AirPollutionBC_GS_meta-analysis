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

# Building Screening but only for women ages 50-71 in England
complete_cohort_screen <- complete_cohort %>%
  filter(
    country == "England"
  ) %>%
  mutate(
    dob = as.Date(date_birth),
    dob_adj = if_else(month(dob) == 2 & day(dob) == 29, dob - 1, dob),  # this deals with the leap year
    start_fu = as.Date(date_fup_start),
    end_fu = as.Date(date_fup_end),
    
    birthday_50 = dob_adj + years(50),
    birthday_71 = dob_adj + years(71),
    
    SCREEN_start_fup = pmax(start_fu, birthday_50, na.rm = TRUE),
    SCREEN_end_fup   = pmin(end_fu, birthday_71, na.rm = TRUE), 
    
    SCREEN_start_age365 = as.numeric(SCREEN_start_fup - dob),
    SCREEN_end_age365   = as.numeric(SCREEN_end_fup - dob), 
  ) %>%
  filter(
    SCREEN_end_age365 > SCREEN_start_age365
  )

cohort_mensplit_screen <- complete_cohort_screen %>% 
  mutate(
    # turn to age in days (no leap years) -- missing becomes 51 * 365
    # add 182 days
    age_meno365 = ifelse((meno_age_est > 12) & (meno_age_est < 60),
                         (meno_age_est * 365) + 182,
                         (51 * 365) + 182),
    # if reason for menopause is related to breast cancer treatment then
    # push age up by one year so it comes after cancer
    # i.e. after censoring, so remains premenopausal at censoring/diagnosis
    # regardless of how much was added on
    age_meno365 = ifelse((event == 1) &
                           (trunc(age_meno365 / 365) ==
                              trunc(c_age365 / 365)) &
                           (meno_reason %in% c(2, 5)),
                         #                               (meno_reason %in% c(2, 3, 4, 5, 6)),
                         age_meno365 + (365 * 1),
                         age_meno365)
  )

# Setting up the base cohort split
cohort_mensplit_screen <- tmerge(data1 = cohort_mensplit_screen, data2 = cohort_mensplit_screen,
                                 id = TCode, tstart = SCREEN_start_age365, tstop = SCREEN_end_age365,
                                 outcome = event(c_age365, event))

# Splitting the cohort by menopause
times <- cohort_mensplit_screen[, c("TCode", "age_meno365")]
cohort_mensplit_screen <- tmerge(data1 = cohort_mensplit_screen, data2 = times,
                                 id = TCode, menoband = tdc(age_meno365))

# Adding menopausal labels
meno_labels <- c("premenopausal", "postmenopausal")
cohort_mensplit_screen$menobandf <- factor(cohort_mensplit_screen$menoband,
                                           levels = 0:1,
                                           labels = meno_labels)

# Adjusting follow up time for each row
cohort_mensplit_screen <- cohort_mensplit_screen %>%
  mutate(
    interval_days = tstop - tstart,
    p_years = interval_days / 365, 
    followup_year = floor((tstart - age_start365) / 365), 
    screen = factor(screen_detected,
                    levels = c(0, 1, 2, 3, 4),
                    labels = c("Nonevent", "Screen Detected", "Interval Cancer", "Other", "Unknown"))
  )


# Screen detected status
yesscreen <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Screen Detected" ~ 1, 
      TRUE ~ 0
    ),
    type = "screen"
  )

noscreen <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Interval Cancer" ~ 1, 
      TRUE ~ 0
    ),
    type = "interval"
  )

other <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Other" ~ 1, 
      TRUE ~ 0
    ),
    type = "other"
  )

unknown <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
screened <- rbind(yesscreen, noscreen, other, unknown)
screened <- screened %>%
  mutate(
    type = factor(type, levels = c("screen", "interval", "other", "unk")),
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )





###################################################################

# Function to take out the HR, CI, and Wald values from the model
extract_lm_hr <- function(cox_fit, exposure_var, subtype_levels = c("pos", "neg", "unk"), exclude_from_wald = "unk") {
  
  model_name <- deparse(substitute(cox_fit))
  
  coefs <- coef(cox_fit)
  vcov_mat <- vcov(cox_fit)
  
  if (!(exposure_var %in% names(coefs))) {
    stop(paste0("Exposure variable '", exposure_var, "' not found in model coefficients"))
  }
  
  b_main <- coefs[exposure_var]
  
  interaction_terms <- grep(paste0("^", exposure_var, ":"), names(coefs), value = TRUE)
  
  # --- Beta extraction ---
  beta <- setNames(rep(0, length(subtype_levels)), subtype_levels)
  beta[subtype_levels[1]] <- b_main
  
  for (subtype in subtype_levels[-1]) {
    pattern <- paste0("type", subtype)
    matching_term <- interaction_terms[grepl(pattern, interaction_terms)]
    if (length(matching_term) == 1) {
      beta[subtype] <- b_main + coefs[matching_term]
    }
  }
  
  # --- SE extraction ---
  se <- setNames(rep(NA, length(subtype_levels)), subtype_levels)
  se[subtype_levels[1]] <- sqrt(vcov_mat[exposure_var, exposure_var])
  
  for (subtype in subtype_levels[-1]) {
    pattern <- paste0("type", subtype)
    matching_term <- interaction_terms[grepl(pattern, interaction_terms)]
    if (length(matching_term) == 1) {
      se[subtype] <- sqrt(
        vcov_mat[exposure_var, exposure_var] +
          vcov_mat[matching_term, matching_term] +
          2 * vcov_mat[exposure_var, matching_term]
      )
    }
  }
  
  # --- Global Wald test (excludes reference level and any in exclude_from_wald) ---
  # Identify interaction terms that correspond to subtypes NOT in exclude_from_wald
  wald_subtypes <- subtype_levels[!subtype_levels %in% c(subtype_levels[1], exclude_from_wald)]
  
  wald_terms <- c()
  for (subtype in wald_subtypes) {
    pattern <- paste0("type", subtype)
    matching_term <- interaction_terms[grepl(pattern, interaction_terms)]
    if (length(matching_term) == 1) {
      wald_terms <- c(wald_terms, matching_term)
    }
  }
  
  wald_pval <- NA
  if (length(wald_terms) >= 1) {
    beta_wald <- coefs[wald_terms]
    vcov_wald <- vcov_mat[wald_terms, wald_terms, drop = FALSE]
    wald_stat  <- t(beta_wald) %*% solve(vcov_wald) %*% beta_wald
    wald_pval  <- pchisq(wald_stat, df = length(wald_terms), lower.tail = FALSE)
  }
  
  # --- Build result ---
  result <- data.frame(
    Model     = model_name,
    Subtype   = subtype_levels,
    HR        = exp(beta),
    CI_lower  = exp(beta - 1.96 * se),
    CI_upper  = exp(beta + 1.96 * se),
    Pval      = 2 * pnorm(abs(beta / se), lower.tail = FALSE),
    Wald_pval = rep(wald_pval, length(subtype_levels))
  )
  
  result <- result %>%
    mutate(
      HR        = round(HR, 2),
      CI_lower  = round(CI_lower, 2),
      CI_upper  = round(CI_upper, 2),
      Pval      = round(Pval, 3),
      Wald_pval = round(Wald_pval, 3)
    )
  
  return(result)
}
