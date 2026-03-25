

# Point

# No Lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointP25 / 5) + matchyear_cat,
  data = complete_cohort
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointP10 / 5) + matchyear_cat,
  data = complete_cohort
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointNO2 / 10) + matchyear_cat,
  data = complete_cohort
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

point_nolag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

point_nolag_clean <- point_nolag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "nolag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "point", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )


# 2 Lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointP25 / 5) + matchyear_cat,
  data = complete_cohort_2lag
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointP10 / 5) + matchyear_cat,
  data = complete_cohort_2lag
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointNO2 / 10) + matchyear_cat,
  data = complete_cohort_2lag
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

point_2lag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

point_2lag_clean <- point_2lag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "2lag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "point", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )

# 5 lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointP25 / 5) + matchyear_cat,
  data = complete_cohort_5lag
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointP10 / 5) + matchyear_cat,
  data = complete_cohort_5lag
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointNO2 / 10) + matchyear_cat,
  data = complete_cohort_5lag
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

point_5lag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

point_5lag_clean <- point_5lag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "5lag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "point", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )

# Average
# No Lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgP25 / 5) + matchyear_cat,
  data = complete_cohort
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgP10 / 5) + matchyear_cat,
  data = complete_cohort
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgNO2 / 10) + matchyear_cat,
  data = complete_cohort
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

avg_nolag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

avg_nolag_clean <- avg_nolag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "nolag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "average", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )

# 2 Lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgP25 / 5) + matchyear_cat,
  data = complete_cohort_2lag
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgP10 / 5) + matchyear_cat,
  data = complete_cohort_2lag
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgNO2 / 10) + matchyear_cat,
  data = complete_cohort_2lag
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_2lag) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_2lag
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_2lag
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

avg_2lag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

avg_2lag_clean <- avg_2lag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "2lag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "average", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )

# 5 Lag
P25_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgP25 / 5) + matchyear_cat,
  data = complete_cohort_5lag
)
P25_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

P10_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgP10 / 5) + matchyear_cat,
  data = complete_cohort_5lag
)
P10_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

NO2_A <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgNO2 / 10) + matchyear_cat,
  data = complete_cohort_5lag
)
NO2_B <- coxph(
  Surv(age_start365, c_age365, bc_5lag) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
  data = complete_cohort_5lag
)
NO2_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit_5lag
)

NO2_A_clean <- extract_cox(NO2_A, "NO2_A", "NO2")
NO2_B_clean <- extract_cox(NO2_B, "NO2_B", "NO2")
NO2_C_clean <- extract_cox(NO2_C, "NO2_C", "NO2")

P25_A_clean <- extract_cox(P25_A, "P25_A", "P25")
P25_B_clean <- extract_cox(P25_B, "P25_B", "P25")
P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")

P10_A_clean <- extract_cox(P10_A, "P10_A", "P10")
P10_B_clean <- extract_cox(P10_B, "P10_B", "P10")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

avg_5lag <- rbind(
  NO2_A_clean, 
  NO2_B_clean, 
  NO2_C_clean,
  
  P25_A_clean,
  P25_B_clean,
  P25_C_clean,
  
  P10_A_clean, 
  P10_B_clean, 
  P10_C_clean
)

avg_5lag_clean <- avg_5lag %>%
  mutate(
    HR = round(HR, 2), 
    Lower_CI = round(Lower_CI, 2), 
    Upper_CI = round(Upper_CI, 2), 
    P_value = round(P_value, 3), 
    Outcome = "5lag", 
    Menopausal_status = "All", 
    `p-value` = P_value,
    Variable = case_when(
      Variable == "P10" ~ "PM10", 
      Variable == "P25" ~ "PM2.5", 
      TRUE ~ Variable
    ), 
    Exposure = case_when(
      Variable == "PM2.5" ~ "per 5", 
      Variable == "PM10" ~ "per 5", 
      Variable == "NO2" ~ "per 10", 
      TRUE ~ "ERROR"
    ), 
    exp_definition = "average", 
    `HR (95% CI)` = sprintf(
      "%.2f (%.2f–%.2f)",
      HR, Lower_CI, Upper_CI
    )
  ) %>%
  dplyr::select(
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome
  )


