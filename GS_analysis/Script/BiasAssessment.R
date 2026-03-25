# Bias Assessment 
library(EValue)

# # PM2.5
# P25_matchyear <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + matchyear_cat,
#   data = complete_cohort
# )
# P25_BMI <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + BMI,
#   data = complete_cohort
# )
# P25_Carstairs <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5)+ Carstairs_cat,
#   data = complete_cohort
# )
# P25_Smoking <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) +Smoking_cat,
#   data = complete_cohort
# )
# P25_Alcohol <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) +Alcohol_cat,
#   data = complete_cohort
# )
# P25_Education <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) +Educ_cat,
#   data = complete_cohort
# )
# P25_Parity <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + Parity_cat,
#   data = complete_cohort
# )
# P25_Birth1 <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) +Birth1_age_cat,
#   data = complete_cohort
# )
# P25_HRT <- coxph(
#   Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(pointP25 / 5) + HRT_cat,
#   data = complete_cohort
# )
# P25_menopause <- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(pointP25 / 5) + menobandf,
#   data = cohort_mensplit
# )
# 
# summary(P25_matchyear) # 1.09 (1.00, 1.19)
# summary(P25_BMI) # 1.09 (1.00, 1.18)
# summary(P25_Carstairs) # 1.10 (1.01, 1.19)
# summary(P25_Smoking) # 1.08 (1.00, 1.17)
# summary(P25_Alcohol) # 1.09 (1.00, 1.18)
# summary(P25_Education) # 1.08 (1.00, 1.17)
# summary(P25_Parity) # 1.08(1.00, 1.17)
# summary(P25_Birth1) # 1.08 (0.99, 1.17)
# summary(P25_HRT) # 1.09(1.00, 1.18)
# summary(P25_menopause) # 1.09 (1.00, 1.18)

# Point estimates
# pointP25<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(pointP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# pointP10<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(pointP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# pointNO2 <- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(pointNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# summary(pointP25) # 1.10 (1.01, 1.20)
# summary(pointP10) # 1.02 (0.96, 1.08)
# summary(pointNO2) #1.03 (0.99, 1.07)
# For P25
  # rare = 1 because outcome is < 15% at follow up and so is classified as rare
  # true = 1 because null hypothesus is no effect
pointP25_evalue <- evalues.HR(est = 1.10, lo = 1.01, hi = 1.20, rare = 1, true = 1) 
# For P10
pointP10_evalue <- evalues.HR(est = 1.02, lo = 0.96, hi = 1.08, rare = 1, true = 1)
# For NO2
pointNO2_evalue <- evalues.HR(est = 1.03, lo = 0.99, hi = 1.07, rare = 1, true = 1)

# Average estimates
# avgP25<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# avgP10<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# avgNO2 <- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(avgNO2 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# summary(avgP25) # 1.12 (1.02, 1.24)
# summary(avgP10) # 1.06 (1.00, 1.13)
# summary(avgNO2) #1.04 (0.99, 1.09)
# For P25
# rare = 1 because outcome is < 15% at follow up and so is classified as rare
# true = 1 because null hypothesus is no effect
avgP25_evalue <- evalues.HR(est = 1.12, lo = 1.02, hi = 1.24, rare = 1, true = 1)
# For P10
avgP10_evalue <- evalues.HR(est = 1.06, lo = 1.00, hi = 1.13, rare = 1, true = 1)
# For NO2
avgNO2_evalue <- evalues.HR(est = 1.04, lo = 0.99, hi = 1.09, rare = 1, true = 1)

# Create a helper function
extract_evalues <- function(evalue_obj, exposure_name) {
  as.data.frame(evalue_obj) |>
    mutate(
      exposure = exposure_name,
      metric = rownames(evalue_obj)
    ) |>
    relocate(exposure, metric)
}

clean_pointP25 <- extract_evalues(pointP25_evalue, "point_PM2.5")
clean_pointP10 <- extract_evalues(pointP10_evalue, "point_PM10")
clean_pointNO2 <- extract_evalues(pointNO2_evalue, "point_NO2")
clean_avgP25 <- extract_evalues(avgP25_evalue, "avg_PM2.5")
clean_avgP10 <- extract_evalues(avgP10_evalue, "avg_PM10")
clean_avgNO2 <- extract_evalues(avgNO2_evalue, "avg_NO2")

all_evalues <- rbind(clean_pointP25, clean_pointP10, clean_pointNO2, clean_avgP25, clean_avgP10, clean_avgNO2) %>%
  filter(
    metric != "RR"
  ) %>%
  mutate(
    e_value = round(point,2), 
    lower = round(lower, 2)
  ) %>%
  select(exposure, e_value, lower, upper)
