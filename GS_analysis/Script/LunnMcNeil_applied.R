### The Lunn McNeil model applied 

## No2
### Per 10
# Model A
A_avgNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) +avgNO2*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_avgNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_avgNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_avgNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_avgNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_avgNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_avgNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_avgNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_avgNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_avgNO2_10_ER_nolag, exposure_var = "avgNO2")
A_2lag <- extract_lm_hr(A_avgNO2_10_ER_2lag, exposure_var = "avgNO2")
A_5lag <- extract_lm_hr(A_avgNO2_10_ER_5lag, exposure_var = "avgNO2")

B_nolag <- extract_lm_hr(B_avgNO2_10_ER_nolag, exposure_var = "avgNO2")
B_2lag <- extract_lm_hr(B_avgNO2_10_ER_2lag, exposure_var = "avgNO2")
B_5lag <- extract_lm_hr(B_avgNO2_10_ER_5lag, exposure_var = "avgNO2")

c_nolag <- extract_lm_hr(C_avgNO2_10_ER_nolag, exposure_var = "avgNO2")
C_2lag <- extract_lm_hr(C_avgNO2_10_ER_2lag, exposure_var = "avgNO2")
C_5lag <- extract_lm_hr(C_avgNO2_10_ER_5lag, exposure_var = "avgNO2")

avgNO2_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)



##################################################

## PM2.5

# Model A
A_avgP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_avgP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_avgP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_avgP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_avgP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_avgP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_avgP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_avgP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_avgP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_avgP25_10_ER_nolag, exposure_var = "avgP25")
A_2lag <- extract_lm_hr(A_avgP25_10_ER_2lag, exposure_var = "avgP25")
A_5lag <- extract_lm_hr(A_avgP25_10_ER_5lag, exposure_var = "avgP25")

B_nolag <- extract_lm_hr(B_avgP25_10_ER_nolag, exposure_var = "avgP25")
B_2lag <- extract_lm_hr(B_avgP25_10_ER_2lag, exposure_var = "avgP25")
B_5lag <- extract_lm_hr(B_avgP25_10_ER_5lag, exposure_var = "avgP25")

c_nolag <- extract_lm_hr(C_avgP25_10_ER_nolag, exposure_var = "avgP25")
C_2lag <- extract_lm_hr(C_avgP25_10_ER_2lag, exposure_var = "avgP25")
C_5lag <- extract_lm_hr(C_avgP25_10_ER_5lag, exposure_var = "avgP25")

avgP25_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)

############

## P10
# Model A
A_avgP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_avgP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_avgP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_avgP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_avgP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_avgP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_avgP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_avgP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_avgP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + avgP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_avgP10_10_ER_nolag, exposure_var = "avgP10")
A_2lag <- extract_lm_hr(A_avgP10_10_ER_2lag, exposure_var = "avgP10")
A_5lag <- extract_lm_hr(A_avgP10_10_ER_5lag, exposure_var = "avgP10")

B_nolag <- extract_lm_hr(B_avgP10_10_ER_nolag, exposure_var = "avgP10")
B_2lag <- extract_lm_hr(B_avgP10_10_ER_2lag, exposure_var = "avgP10")
B_5lag <- extract_lm_hr(B_avgP10_10_ER_5lag, exposure_var = "avgP10")

c_nolag <- extract_lm_hr(C_avgP10_10_ER_nolag, exposure_var = "avgP10")
C_2lag <- extract_lm_hr(C_avgP10_10_ER_2lag, exposure_var = "avgP10")
C_5lag <- extract_lm_hr(C_avgP10_10_ER_5lag, exposure_var = "avgP10")

avgP10_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)


###############################################
## No2
### Per 10
# Model A
A_pointNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_pointNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_pointNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_pointNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_pointNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_pointNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_pointNO2_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_pointNO2_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_pointNO2_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointNO2*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_pointNO2_10_ER_nolag, exposure_var = "pointNO2")
A_2lag <- extract_lm_hr(A_pointNO2_10_ER_2lag, exposure_var = "pointNO2")
A_5lag <- extract_lm_hr(A_pointNO2_10_ER_5lag, exposure_var = "pointNO2")

B_nolag <- extract_lm_hr(B_pointNO2_10_ER_nolag, exposure_var = "pointNO2")
B_2lag <- extract_lm_hr(B_pointNO2_10_ER_2lag, exposure_var = "pointNO2")
B_5lag <- extract_lm_hr(B_pointNO2_10_ER_5lag, exposure_var = "pointNO2")

c_nolag <- extract_lm_hr(C_pointNO2_10_ER_nolag, exposure_var = "pointNO2")
C_2lag <- extract_lm_hr(C_pointNO2_10_ER_2lag, exposure_var = "pointNO2")
C_5lag <- extract_lm_hr(C_pointNO2_10_ER_5lag, exposure_var = "pointNO2")

pointNO2_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)



##################################################

## PM2.5

# Model A
A_pointP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_pointP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_pointP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_pointP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_pointP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_pointP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_pointP25_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_pointP25_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_pointP25_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP25*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_pointP25_10_ER_nolag, exposure_var = "pointP25")
A_2lag <- extract_lm_hr(A_pointP25_10_ER_2lag, exposure_var = "pointP25")
A_5lag <- extract_lm_hr(A_pointP25_10_ER_5lag, exposure_var = "pointP25")

B_nolag <- extract_lm_hr(B_pointP25_10_ER_nolag, exposure_var = "pointP25")
B_2lag <- extract_lm_hr(B_pointP25_10_ER_2lag, exposure_var = "pointP25")
B_5lag <- extract_lm_hr(B_pointP25_10_ER_5lag, exposure_var = "pointP25")

c_nolag <- extract_lm_hr(C_pointP25_10_ER_nolag, exposure_var = "pointP25")
C_2lag <- extract_lm_hr(C_pointP25_10_ER_2lag, exposure_var = "pointP25")
C_5lag <- extract_lm_hr(C_pointP25_10_ER_5lag, exposure_var = "pointP25")

pointP25_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)

############

## P10
# Model A
A_pointP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)


A_pointP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

A_pointP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model B
B_pointP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_nolag$TCode,
  data = AB_lm_nolag
)

B_pointP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_2lag$TCode,
  data = AB_lm_2lag
)

B_pointP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat,
  cluster = AB_lm_5lag$TCode,
  data = AB_lm_5lag
)

# Model C
C_pointP10_10_ER_nolag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat + Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_nolag$TCode,
  data = C_lm_nolag
)

C_pointP10_10_ER_2lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat+ BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_2lag$TCode,
  data = C_lm_2lag
)

C_pointP10_10_ER_5lag <- coxph(
  Surv(age_start365, c_age365, bc_nolag) ~ strata(type) + pointP10*type + matchyear_cat + BMI +Carstairs_cat +Smoking_cat + Alcohol_cat+Educ_cat+ Parity_cat+Birth1_age_cat +HRT_cat +menobandf,
  cluster = C_lm_5lag$TCode,
  data = C_lm_5lag
)


A_nolag <- extract_lm_hr(A_pointP10_10_ER_nolag, exposure_var = "pointP10")
A_2lag <- extract_lm_hr(A_pointP10_10_ER_2lag, exposure_var = "pointP10")
A_5lag <- extract_lm_hr(A_pointP10_10_ER_5lag, exposure_var = "pointP10")

B_nolag <- extract_lm_hr(B_pointP10_10_ER_nolag, exposure_var = "pointP10")
B_2lag <- extract_lm_hr(B_pointP10_10_ER_2lag, exposure_var = "pointP10")
B_5lag <- extract_lm_hr(B_pointP10_10_ER_5lag, exposure_var = "pointP10")

c_nolag <- extract_lm_hr(C_pointP10_10_ER_nolag, exposure_var = "pointP10")
C_2lag <- extract_lm_hr(C_pointP10_10_ER_2lag, exposure_var = "pointP10")
C_5lag <- extract_lm_hr(C_pointP10_10_ER_5lag, exposure_var = "pointP10")

pointP10_lm <- rbind(
  A_nolag, 
  A_2lag,
  A_5lag,
  B_nolag,
  B_2lag,
  B_5lag,
  c_nolag,
  C_2lag,
  C_5lag
)

lunnmcneil_results <- rbind(
  avgNO2_lm, 
  avgP25_lm, 
  avgP10_lm, 
  
  pointNO2_lm, 
  pointP25_lm, 
  pointP10_lm
)
