proportionalhazards <- function (inputdata, meno_inputdata){
  # No lag point models
  
  P25_A <- coxph(
    Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(avgP25 / 5) + matchyear_cat,
    data = inputdata
  )
  P25_B <- coxph(
    Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
    data = inputdata
  )
  P25_C <- coxph(
    Surv(tstart/365, tstop/365, outcome) ~ I(avgP25 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf,
    data = meno_inputdata
  )
  
  P10_A <- coxph(
    Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(avgP10 / 5) + matchyear_cat,
    data = inputdata
  )
  P10_B <- coxph(
    Surv(age_start365/365, c_age365/365, bc_nolag) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat,
    data = inputdata
  )
  P10_C <- coxph(
    Surv(tstart/365, tstop/365, outcome) ~ I(avgP10 / 5) + matchyear_cat + BMI + Carstairs_cat+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf,
    data = meno_inputdata
  )
  
  
  # Proportional hazard testing: 
  
  ph_P25_A <- cox.zph(P25_A)
  ph_P25_B <- cox.zph(P25_B)
  ph_P25_C <- cox.zph(P25_C)
  
  ph_P10_A <- cox.zph(P10_A)
  ph_P10_B <- cox.zph(P10_B)
  ph_P10_C <- cox.zph(P10_C)
  
  
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
  )
  
  ph_summary_clean <- ph_summary %>%
    mutate(
      p_value = round(p_value, 3), 
      variable = case_when(
        grepl("avg", variable) ~ "Main Exposure",
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
  return(ph_summary_clean)
  
}
