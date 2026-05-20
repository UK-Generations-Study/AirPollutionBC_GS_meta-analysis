# Bias Assessment 
library(EValue)


#Average estimates
# avgP25<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(avgP25 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# avgP10<- coxph(
#   Surv(tstart/365, tstop/365, outcome) ~ I(avgP10 / 10) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf+ cluster(TCode),
#   data = cohort_mensplit
# )
# summary(avgP25) # 1.26 (1.04, 1.53)
# summary(avgP10) # 1.13 (0.99, 1.28)
# For P25
# rare = 1 because outcome is < 15% at follow up and so is classified as rare
# true = 1 because null hypothesus is no effect
avgP25_evalue <- evalues.HR(est = 1.26, lo = 1.04, hi = 1.53, rare = 1, true = 1)
# For P10
avgP10_evalue <- evalues.HR(est = 1.13, lo = 0.99, hi = 1.28, rare = 1, true = 1)

# Create a helper function
extract_evalues <- function(evalue_obj, exposure_name) {
  as.data.frame(evalue_obj) |>
    mutate(
      exposure = exposure_name,
      metric = rownames(evalue_obj)
    ) |>
    relocate(exposure, metric)
}

clean_avgP25 <- extract_evalues(avgP25_evalue, "avg_PM2.5")
clean_avgP10 <- extract_evalues(avgP10_evalue, "avg_PM10")

all_evalues <- rbind(clean_avgP25, clean_avgP10) %>%
  filter(
    metric != "RR"
  ) %>%
  mutate(
    e_value = round(point,2), 
    lower = round(lower, 2)
  ) %>%
  select(exposure, e_value, lower, upper)
