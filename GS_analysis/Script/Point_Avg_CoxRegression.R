# Average
# No Lag
P25_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_quint  +Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)
P10_C <- coxph(
  Surv(tstart, tstop, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_quint +Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
  data = cohort_mensplit
)


P25_C_clean <- extract_cox(P25_C, "P25_C", "P25")
P10_C_clean <- extract_cox(P10_C, "P10_C", "P10")

avg_nolag <- rbind(
  P25_C_clean,
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
    Model, Variable, exp_definition, Exposure, Menopausal_status, `HR (95% CI)`, `p-value`, Outcome, HR, Lower_CI, Upper_CI
  )

