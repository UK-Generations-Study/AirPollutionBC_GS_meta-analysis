### The Lunn McNeil model applied 

## PM2.5

# ER
ER_Status <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = er_status$TCode,
  data = er_status
)
ER_LM <- extract_lm_hr(ER_Status, exposure_var = "avgP25", subtype_levels = c("pos", "neg", "unk"))

# Screen Detected
Screen_Detected <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = screened$TCode,
  data = screened
)
Screen_LM <- extract_lm_hr(Screen_Detected, exposure_var = "avgP25", subtype_levels = c("screen", "noscreen", "unk"))

# Invasive vs DCIS
Invasive_DCIS <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = invdcis_data$TCode,
  data = invdcis_data
)
InvasiveDCIS_LM <- extract_lm_hr(Invasive_DCIS, exposure_var = "avgP25", subtype_levels = c("invasive", "dcis", "unk"))


avgP25_lm <- rbind(ER_LM, Screen_LM, InvasiveDCIS_LM) %>%
  mutate(
    Exposure = "PM 2.5"
  )
############

## P10
# ER
ER_Status <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = er_status$TCode,
  data = er_status
)
ER_LM <- extract_lm_hr(ER_Status, exposure_var = "avgP10", subtype_levels = c("pos", "neg", "unk"))

# Screen Detected
Screen_Detected <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = screened$TCode,
  data = screened
)
Screen_LM <- extract_lm_hr(Screen_Detected, exposure_var = "avgP10", subtype_levels = c("screen", "noscreen", "unk"))

# Invasive vs DCIS
Invasive_DCIS <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = invdcis_data$TCode,
  data = invdcis_data
)
InvasiveDCIS_LM <- extract_lm_hr(Invasive_DCIS, exposure_var = "avgP10", subtype_levels = c("invasive", "dcis", "unk"))


avgP10_lm <- rbind(ER_LM, Screen_LM, InvasiveDCIS_LM) %>%
  mutate(
    Exposure = "PM 10"
  )
###############################################

lunnmcneil_results <- rbind(
  avgP25_lm, 
  avgP10_lm
)
