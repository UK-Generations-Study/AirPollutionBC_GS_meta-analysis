# No lag point models

P25_A <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + matchyear_cat,
  data = complete_cohort
)
P25_B <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P25_C <- coxph(
  Surv(tstart/365, tstop/365, outcome) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf,
  data = cohort_mensplit
)

P10_A <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP10 / 5) + matchyear_cat,
  data = complete_cohort
)
P10_B <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P10_C <- coxph(
  Surv(tstart/365, tstop/365, outcome) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf,
  data = cohort_mensplit
)

NO2_A <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointNO2 / 10) + matchyear_cat,
  data = complete_cohort
)
NO2_B <- coxph(
  Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
NO2_C <- coxph(
  Surv(tstart/365, tstop/365, outcome) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf,
  data = cohort_mensplit
)

# Proportional hazard testing: 

ph_P25_A <- cox.zph(P25_A)
ph_P25_B <- cox.zph(P25_B)
ph_P25_C <- cox.zph(P25_C)

ph_P10_A <- cox.zph(P10_A)
ph_P10_B <- cox.zph(P10_B)
ph_P10_C <- cox.zph(P10_C)

ph_NO2_A <- cox.zph(NO2_A)
ph_NO2_B <- cox.zph(NO2_B)
ph_NO2_C <- cox.zph(NO2_C)

# Prooportional hazard equation for saving results
extract_ph <- function(ph_model, model_name) {
  tibble(
    model = model_name,
    variable = rownames(ph_model$table),
    chisq = ph_model$table[,"chisq"],
    df = ph_model$table[,"df"],
    p_value = ph_model$table[,"p"]
  )
}

# Combine results for all models
ph_summary <- bind_rows(
  extract_ph(ph_P25_A, "P25_A"),
  extract_ph(ph_P25_B, "P25_B"),
  extract_ph(ph_P25_C, "P25_C"), 
  
  extract_ph(ph_P10_A, "P10_A"),
  extract_ph(ph_P10_B, "P10_B"),
  extract_ph(ph_P10_C, "P10_C"),
  
  extract_ph(ph_NO2_A, "NO2_A"),
  extract_ph(ph_NO2_B, "NO2_B"),
  extract_ph(ph_NO2_C, "NO2_C")
)

ph_summary_clean <- ph_summary %>%
  mutate(
    p_value = round(p_value, 3), 
    variable = case_when(
      grepl("point", variable) ~ "Main Exposure",
      TRUE ~ variable
    )
  )%>%
  pivot_wider(
    id_cols = model,
    names_from = variable,
    values_from = p_value
  ) %>%
  dplyr::select(
    model,
    `Main Exposure`,
    GLOBAL,
    everything()
  )


