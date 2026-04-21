

C_P25_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP25 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ Parity_cat + Birth1_age_cat + HRT_cat + cluster(TCode) , 
                   data = cohort_mensplit_nolag_changed)

C_P10_int <- coxph(Surv(tstart/365, tstop/365, outcome) ~ avgP10 * menobandf + menobandf + matchyear_cat + BMI + Carstairs_cat + Smoking_cat + Alcohol_cat+ Parity_cat + Birth1_age_cat + HRT_cat+ cluster(TCode), 
                   data = cohort_mensplit_nolag_changed)

interaction_HR <- rbind(
  extract_hr_ci_avg(C_P25_int, "C_P25_int"),
  extract_hr_ci_avg(C_P10_int, "C_P10_int")
) 

wald_pval <- rbind(
  wald_pvalue(C_P25_int, "avgP25:menobandfpostmenopausal","C_P25_int"),
  wald_pvalue(C_P10_int, "avgP10:menobandfpostmenopausal","C_P10_int")
)

all_men_results <- interaction_HR %>%
  left_join(wald_pval, by = c("Model", "pollutant")) %>%
  dplyr::select(-term.x, -term.y) %>%
  pivot_longer(
    cols = c(HR_pre, HR_post,
             CI_lower_pre, CI_lower_post,
             CI_upper_pre, CI_upper_post,
             pvalue_pre, pvalue_post),
    names_to = c(".value", "Subtype"),
    names_pattern = "(.*)_(pre|post)"
  ) %>%
  mutate(
    Model = "Menopausal_Status", 
    Exposure = case_when(
      pollutant == "P25" ~ "PM 2.5", 
      pollutant == "P10" ~ "PM 10", 
      TRUE ~ "ERROR"
    ), 
    Pval = pvalue, 
    Wald_pval = wald_pval
  ) %>%
  select(Model, Subtype, HR, CI_lower, CI_upper, Pval, Wald_pval, Exposure)
