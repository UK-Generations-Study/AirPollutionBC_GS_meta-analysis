
A_P25_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP25 * menobandf + menobandf + matchyear_cat+ cluster(TCode), 
                   data = cohort_mensplit_5lag_changed)
B_P25_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP25 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ cluster(TCode), 
                   data = cohort_mensplit_5lag_changed)
C_P25_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP25 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ Parity_cat + Birth1_age_cat + HRT_cat+ cluster(TCode)  , 
                   data = cohort_mensplit_5lag_changed)

A_P10_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP10 * menobandf + menobandf + matchyear_cat+ cluster(TCode), 
                   data = cohort_mensplit_5lag_changed)
B_P10_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP10 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ cluster(TCode), 
                   data = cohort_mensplit_5lag_changed)
C_P10_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP10 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ Parity_cat + Birth1_age_cat + HRT_cat+ cluster(TCode), 
                   data = cohort_mensplit_5lag_changed)


interaction_HR <- rbind(
  extract_hr_ci_avg(A_P25_int, "A_P25_int"),
  extract_hr_ci_avg(B_P25_int, "B_P25_int"),
  extract_hr_ci_avg(C_P25_int, "C_P25_int"),
  
  extract_hr_ci_avg(A_P10_int, "A_P10_int"),
  extract_hr_ci_avg(B_P10_int, "B_P10_int"),
  extract_hr_ci_avg(C_P10_int, "C_P10_int")
) 



wald_pval <- rbind(
  wald_pvalue(A_P25_int, "avgP25:menobandfpostmenopausal","A_P25_int"),
  wald_pvalue(B_P25_int, "avgP25:menobandfpostmenopausal","B_P25_int"),
  wald_pvalue(C_P25_int, "avgP25:menobandfpostmenopausal","C_P25_int"),
  
  wald_pvalue(A_P10_int, "avgP10:menobandfpostmenopausal","A_P10_int"),
  wald_pvalue(B_P10_int, "avgP10:menobandfpostmenopausal","B_P10_int"),
  wald_pvalue(C_P10_int, "avgP10:menobandfpostmenopausal","C_P10_int")
)

all_men_results <- interaction_HR %>%
  left_join(wald_pval, by = c("Model", "pollutant")) %>%
  mutate(
    wald_pval = round(wald_pval, 3), 
    HR_pre = round(HR_pre, 2), 
    CI_lower_pre = round(CI_lower_pre, 2),
    CI_upper_pre = round(CI_upper_pre, 2),
    pvalue_pre = round(pvalue_pre, 3),
    HR_post = round(HR_post, 2), 
    CI_lower_post = round(CI_lower_post, 2),
    CI_upper_post = round(CI_upper_post, 2),
    pvalue_post = round(pvalue_post, 3)
  )

avg_5lag <- all_men_results %>%
  dplyr::select(-term.x, -term.y) %>%
  mutate(
    exposure = "average", 
    units = case_when(
      pollutant =="NO2" ~ "per10", 
      TRUE ~"per5"), 
    lag = "5 years"
  )
