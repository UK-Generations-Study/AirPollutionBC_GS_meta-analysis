# Cleaning the exposure data for Senstivity Analysis 
  # 1. Only use HH geocoded addresses
  # 2. Only people who did not move


# Clean in the data 
data_ap <- read_csv("R:\\Shared\\GENERATIONS\\GeoLinkages\\GSalladdresses_Expanselink.csv")
data_ap <- data_ap %>%
  dplyr::select(-`...1`)

data_gs <- read_csv("R:/Shared/GENERATIONS/GeoLinkages/Geocoded_data/FinalData/GS_allgeocoded.csv")

onlyHH_GS <- data_gs %>%
  filter(
    Source == "HH"
  )

nomove_GS <- data_gs %>%
  group_by(TCode) %>%
  filter(n() == 1) %>%
  ungroup()

sensitivity_data_clean <- function(GS_data, AirPollution_data) {
  
  # ── Step 1: Prep GS data ───────────────────────────────────────────────────
  data_gs <- GS_data %>%
    mutate(TCode = paste(TCode, mnum, sep = "-")) %>%
    dplyr::select(TCode, UpdatedWhen)
  
  # ── Step 2: Join and define time windows ───────────────────────────────────
  data_all <- data_gs %>%
    left_join(AirPollution_data, by = "TCode") %>%
    mutate(TCode_mult = str_remove(TCode, "-.*"),
           startyr    = year(UpdatedWhen)) %>%
    arrange(TCode_mult, UpdatedWhen) %>%
    group_by(TCode_mult) %>%
    mutate(endyr = coalesce(year(lead(UpdatedWhen)), 2020L)) %>%
    ungroup()
  
  # ── Step 3: Identify pollutant-year columns ────────────────────────────────
  # Everything that isn't a metadata column
  meta_cols  <- c("TCode", "TCode_mult", "UpdatedWhen", "startyr", "endyr")
  pollut_cols <- setdiff(names(data_all), meta_cols)
  
  # ── Step 4: Apply time-window filter WITHOUT pivoting ─────────────────────
  # Extract the year suffix from each column name and null out values
  # that fall outside [startyr, endyr) — all in one vectorised pass
  
  data_filtered <- data_all
  
  for (col in pollut_cols) {
    # Extract 2-digit year suffix (e.g. "04" from "NO2_04")
    yr_suffix <- str_extract(col, "\\d{2}$")
    
    if (!is.na(yr_suffix)) {
      full_yr <- as.integer(paste0("20", yr_suffix))
      # Vectorised: set to NA where year is outside the address window
      data_filtered[[col]] <- ifelse(
        data_filtered$startyr <= full_yr & full_yr < data_filtered$endyr,
        data_filtered[[col]],
        NA_real_
      )
    }
  }
  
  # ── Step 5: Collapse multiple addresses per person ─────────────────────────
  # Use data.table for speed if available, otherwise dplyr
  data_cleaned <- data_filtered %>%
    dplyr::select(all_of(c("TCode_mult", pollut_cols))) %>%
    group_by(TCode_mult) %>%
    summarise(
      across(all_of(pollut_cols),
             \(x) x[which.min(is.na(x))][1]),   # first non-NA, faster than first(na.omit())
      .groups = "drop"
    ) %>%
    rename(TCode = TCode_mult)
  
  return(data_cleaned)
}

data_prep <- function(data, title = "UNDEFINED",redefined = F, lag = "none", exposure = "average"){

  data_clean <- as.data.frame(data)
  
  if(redefined == T){
  complete_cohort <- cleanexposure(data_clean, clean_covariates)
  }
  
  complete_cohort <- complete_cohort %>%
    filter(
      !is.na(BMI)
    ) %>%
    dplyr::filter(
      !if_all(NO2_04:NO2_19, is.na)
      ) %>%
    mutate(
      meno_age_est = as.numeric(meno_age_est)
    ) 
  
  if(lag == "none") {
  
  # Menopausal time split
  cohort_mensplit <- complete_cohort %>% 
    mutate(
      # turn to age in days (no leap years) -- missing becomes 51 * 365
      # add 182 days
      age_meno365 = ifelse((meno_age_est > 12) & (meno_age_est < 60),
                           (meno_age_est * 365) + 182,
                           (51 * 365) + 182),
      # if reason for menopause is related to breast cancer treatment then
      # push age up by one year so it comes after cancer
      # i.e. after censoring, so remains premenopausal at censoring/diagnosis
      # regardless of how much was added on
      age_meno365 = ifelse((event == 1) &
                             (trunc(age_meno365 / 365) ==
                                trunc(c_age365 / 365)) &
                             (meno_reason %in% c(2, 5)),
                           #                               (meno_reason %in% c(2, 3, 4, 5, 6)),
                           age_meno365 + (365 * 1),
                           age_meno365)
    )
  
  # Setting up the base cohort split
  cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = cohort_mensplit,
                            id = TCode, tstart = age_start365, tstop = c_age365,
                            outcome = event(c_age365, event))
  
  # Splitting the cohort by menopause
  times <- cohort_mensplit[, c("TCode", "age_meno365")]
  cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = times,
                            id = TCode, menoband = tdc(age_meno365))
  
  # Adding menopausal labels
  meno_labels <- c("premenopausal", "postmenopausal")
  cohort_mensplit$menobandf <- factor(cohort_mensplit$menoband,
                                      levels = 0:1,
                                      labels = meno_labels)
  
  # Adjusting follow up time for each row
  cohort_mensplit <- cohort_mensplit %>%
    mutate(
      interval_days = tstop - tstart,
      p_years = interval_days / 365, 
      followup_year = floor((tstart - age_start365) / 365)
    )
  } else if(lag == "2 lag") {
    # Menopausal time split
    cohort_mensplit <- complete_cohort_2lag %>% 
      mutate(
        # turn to age in days (no leap years) -- missing becomes 51 * 365
        # add 182 days
        age_meno365 = ifelse((meno_age_est > 12) & (meno_age_est < 60),
                             (meno_age_est * 365) + 182,
                             (51 * 365) + 182),
        # if reason for menopause is related to breast cancer treatment then
        # push age up by one year so it comes after cancer
        # i.e. after censoring, so remains premenopausal at censoring/diagnosis
        # regardless of how much was added on
        age_meno365 = ifelse((event == 1) &
                               (trunc(age_meno365 / 365) ==
                                  trunc(c_age365 / 365)) &
                               (meno_reason %in% c(2, 5)),
                             #                               (meno_reason %in% c(2, 3, 4, 5, 6)),
                             age_meno365 + (365 * 1),
                             age_meno365)
      )
    
    # Setting up the base cohort split
    cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = cohort_mensplit,
                              id = TCode, tstart = age_start365, tstop = c_age365,
                              outcome = event(c_age365, event))
    
    # Splitting the cohort by menopause
    times <- cohort_mensplit[, c("TCode", "age_meno365")]
    cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = times,
                              id = TCode, menoband = tdc(age_meno365))
    
    # Adding menopausal labels
    meno_labels <- c("premenopausal", "postmenopausal")
    cohort_mensplit$menobandf <- factor(cohort_mensplit$menoband,
                                        levels = 0:1,
                                        labels = meno_labels)
    
    # Adjusting follow up time for each row
    cohort_mensplit <- cohort_mensplit %>%
      mutate(
        interval_days = tstop - tstart,
        p_years = interval_days / 365, 
        followup_year = floor((tstart - age_start365) / 365)
      )
  }else if(lag == "5 lag") {
    # Menopausal time split
    cohort_mensplit <- complete_cohort_5lag %>% 
      mutate(
        # turn to age in days (no leap years) -- missing becomes 51 * 365
        # add 182 days
        age_meno365 = ifelse((meno_age_est > 12) & (meno_age_est < 60),
                             (meno_age_est * 365) + 182,
                             (51 * 365) + 182),
        # if reason for menopause is related to breast cancer treatment then
        # push age up by one year so it comes after cancer
        # i.e. after censoring, so remains premenopausal at censoring/diagnosis
        # regardless of how much was added on
        age_meno365 = ifelse((event == 1) &
                               (trunc(age_meno365 / 365) ==
                                  trunc(c_age365 / 365)) &
                               (meno_reason %in% c(2, 5)),
                             #                               (meno_reason %in% c(2, 3, 4, 5, 6)),
                             age_meno365 + (365 * 1),
                             age_meno365)
      )
    
    # Setting up the base cohort split
    cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = cohort_mensplit,
                              id = TCode, tstart = age_start365, tstop = c_age365,
                              outcome = event(c_age365, event))
    
    # Splitting the cohort by menopause
    times <- cohort_mensplit[, c("TCode", "age_meno365")]
    cohort_mensplit <- tmerge(data1 = cohort_mensplit, data2 = times,
                              id = TCode, menoband = tdc(age_meno365))
    
    # Adding menopausal labels
    meno_labels <- c("premenopausal", "postmenopausal")
    cohort_mensplit$menobandf <- factor(cohort_mensplit$menoband,
                                        levels = 0:1,
                                        labels = meno_labels)
    
    # Adjusting follow up time for each row
    cohort_mensplit <- cohort_mensplit %>%
      mutate(
        interval_days = tstop - tstart,
        p_years = interval_days / 365, 
        followup_year = floor((tstart - age_start365) / 365)
      )
  } else {
    stop("options are: none, 2 lag, 5 lag")
  }
  
  if (exposure == "average"){
  P10_C <- coxph(
    Surv(tstart, tstop, outcome) ~ I(avgP10 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
    data = cohort_mensplit
  )
  P25_C <- coxph(
    Surv(tstart, tstop, outcome) ~ I(avgP25 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
    data = cohort_mensplit
  )
  } else if (exposure == "point") {
    P10_C <- coxph(
      Surv(tstart, tstop, outcome) ~ I(pointP10 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
      data = cohort_mensplit
    )
    P25_C <- coxph(
      Surv(tstart, tstop, outcome) ~ I(pointP25 / 10) + matchyear_cat + BMI + Carstairs_quint+Smoking_cat+Alcohol_cat+Educ_cat+Parity_cat+Birth1_age_cat+HRT_cat+menobandf + cluster(TCode),
      data = cohort_mensplit
    )
  } else{
    stop("options are: average, point")
  }
  
  P10_sensitivity <- extract_cox(P10_C, "P10_C", "P10") %>%
    mutate(
      Model = "PM 10",
      `Bias Adjustment` = title,
      Covariate = "All", 
      `HR` = HR,
      `Lower CI` = Lower_CI,
      `Upper CI`= Upper_CI, 
      `P-value` = P_value
    ) %>%
    select(Model, `Bias Adjustment`, Covariate, `HR`, `Lower CI`,`Upper CI`, `P-value`)%>%
    mutate(
      HR = round(HR, 2), 
      `Lower CI` = round(`Lower CI`, 2), 
      `Upper CI` = round(`Upper CI`, 2),
      `P-value` = round(`P-value`, 3)
    )
  
  P25_sensitivity <- extract_cox(P25_C, "P25_C", "P25")%>%
    mutate(
      Model = "PM 2.5", 
      `Bias Adjustment` = title,
      Covariate = "All", 
      `HR` = HR,
      `Lower CI` = Lower_CI,
      `Upper CI`= Upper_CI, 
      `P-value` = P_value
    ) %>%
    select(Model, `Bias Adjustment`, Covariate, `HR`, `Lower CI`,`Upper CI`, `P-value`)%>%
    mutate(
      HR = round(HR, 2), 
      `Lower CI` = round(`Lower CI`, 2), 
      `Upper CI` = round(`Upper CI`, 2),
      `P-value` = round(`P-value`, 3)
    )
  
  return(
    list(
    P10_sensitivity = P10_sensitivity, 
    P25_sensitivity = P25_sensitivity
    )
  )
}

# Covariate at a time 

sensitivity_covariate <- function(covariate, title = "NOT DEFINED") {
  
  fml_P10 <- as.formula(
    paste("Surv(tstart, tstop, outcome) ~ I(avgP10 / 10) +", covariate, "+ cluster(TCode)")
  )
  
  fml_P25 <- as.formula(
    paste("Surv(tstart, tstop, outcome) ~ I(avgP25 / 10) +", covariate, "+ cluster(TCode)")
  )
  
  P10_C <- coxph(fml_P10, data = cohort_mensplit)
  P25_C <- coxph(fml_P25, data = cohort_mensplit)
  
  P10_sensitivity <- extract_cox(P10_C, "P10_C", "P10") %>%
    mutate(
      Model = "PM 10",
      `Bias Adjustment` = "Single Covariate Analysis",
      Covariate = title,
      `HR` = HR,
      `Lower CI` = Lower_CI,
      `Upper CI`= Upper_CI, 
      `P-value` = P_value
    ) %>%
    select(Model, `Bias Adjustment`, Covariate, `HR`, `Lower CI`,`Upper CI`, `P-value`)%>%
    mutate(
      HR = round(HR, 2), 
      `Lower CI` = round(`Lower CI`, 2), 
      `Upper CI` = round(`Upper CI`, 2),
      `P-value` = round(`P-value`, 3)
    )
  
  P25_sensitivity <- extract_cox(P25_C, "P25_C", "P25")%>%
    mutate(
      Model = "PM 2.5", 
      `Bias Adjustment` = "Single Covariate Analysis",
      Covariate = title,
      `HR` = HR,
      `Lower CI` = Lower_CI,
      `Upper CI`= Upper_CI, 
      `P-value` = P_value
    ) %>%
    select(Model, `Bias Adjustment`, Covariate, `HR`, `Lower CI`,`Upper CI`, `P-value`)%>%
    mutate(
      HR = round(HR, 2), 
      `Lower CI` = round(`Lower CI`, 2), 
      `Upper CI` = round(`Upper CI`, 2),
      `P-value` = round(`P-value`, 3)
    )
  
  return(
    list(
      P10_sensitivity = P10_sensitivity, 
      P25_sensitivity = P25_sensitivity
    )
  )
}


# Sensitivity Analysis: 

sensitivity_unadjusted <- sensitivity_covariate("", "Unadjusted")
sensitivity_matchyear <- sensitivity_covariate("matchyear_cat", "Year of Study Entry")
sensitivity_BMI <- sensitivity_covariate("BMI", "BMI")
sensitivity_Carstairs <- sensitivity_covariate("Carstairs_quint", "Carstairs Index")
sensitivity_Smoking <- sensitivity_covariate("Smoking_cat", "Smoking")
sensitivity_Alcohol <- sensitivity_covariate("Alcohol_cat", "Alcohol")
sensitivity_Educ <- sensitivity_covariate("Educ_cat", "Education")
sensitivity_Parity <- sensitivity_covariate("Parity_cat", "Parity")
sensitivity_1Birth <- sensitivity_covariate("Birth1_age_cat", "Age at First Birth")
sensitivity_HRT <- sensitivity_covariate("HRT_cat", "HRT use")
sensitivity_Meno <- sensitivity_covariate("menobandf", "Menopause")

## ONLY HOUSEHOLD ADDRESSES
clean_onlyHH_GS <- sensitivity_data_clean(onlyHH_GS, data_ap)
sensitivity_household <- data_prep(clean_onlyHH_GS, "Addresses at the Household Level Only", redefined = T)

# ## ONLY PARTICIPANTS WHO DID NOT MOVE
# clean_nomove_GS <- sensitivity_data_clean(nomove_GS, data_ap)
# sensitivity_nomove <- data_prep(clean_nomove_GS, "Participants who did not move Only", redefined = T)

## Point Exposure
sensitivity_point <- data_prep(complete_cohort, "Point Exposure", exposure = "point")

## 2 lag
sensitivity_2lag <- data_prep(complete_cohort, "2 year time lag", lag = "2 lag")

## 5 lag
sensitivity_5lag <- data_prep(complete_cohort, "5 year time lag", lag = "5 lag")
