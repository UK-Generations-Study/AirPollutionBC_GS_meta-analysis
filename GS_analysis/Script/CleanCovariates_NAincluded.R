# This code cleans the covariates for the air pollution analysis and creates the final input dataset

base <- base %>%
  mutate(
    # Defining the date of birth 
    ADOB_F = ADOB_F.x
  )
base_all <- base %>% 
  # adding new variables on reproductive factors
  add_identification_data() %>% 
  append_oc_data() %>% 
  append_menarche_data() %>% 
  add_parity_data()

# clean the carstairs data 
carstairs <- data_carstairs %>%
  dplyr::select(TCode, carstairs, car_dec)


# Entry Year
entryyr <- data_realyears %>%
  select(TCode, Entry) %>%
  # Remove any participants where there is no entry date (They did not completely enter the study)
  filter(Entry != "NULL") %>%
  mutate(
    # Clean the Entry date
    # Specifically - if the entry date is in 2003, reset to 2004 as this is the official study start year
    Entry = trimws(Entry),
    matchyear = case_when(
      Entry == "2003" ~ "2004",
      TRUE ~ Entry
    ),
    # 2004 -> 04
    suffix = str_sub(matchyear, -2)
  )


# Cleaning the cohort data with outcome and covariates only  
clean_cohort <- subset(
  base_all,
  select = c(
    "TCode",
    "date_birth",
    "date_entry",
    "date_fup_start",
    "date_fup_end",
    "age_start365",
    "age_end365",
    "date_diag",
    "diagyear",
    "diagage365",
    "ICDt",
    "event", #0/1 if breast cancer 
    "er_Status", 
    "diagage",
    "c_age365", #person years of followup
    "bodysizebminow", 
    "ethnew5", 
    "menarcheage", 
    "hrtever",
    "fambrca", 
    "R1menopause",
    "meno_age_est", 
    "R1smokingstatus", 
    "R1alcoholstatus", 
    "x_age_birth_last",
    "AgeatEntry", 
    "x_parity", 
    "x_age_birth_1", 
    "educage", 
    #"c_age",
    "event_age",
    "followup_years_age",
    "start_time365", 
    "end_time365",
    "survival_time",
    "event_surv",
    "followup_years_surv",
    "meno_reason"
  )
)



# Creating event time lag variables 
# Adding carstairs score
clean_cohort <- clean_cohort %>% 
  mutate(TCode = as.character(TCode)) %>%
  left_join(carstairs, by = "TCode") %>%
  left_join(entryyr, by = "TCode") %>%
  mutate(
    er_Status = trimws(er_Status),
    event_er = case_when(
      er_Status == "Positive" & event == 1 ~ 1, # Positive
      er_Status == "Unknown" & event == 1 ~ 3, # Unknown
      er_Status == "Negative" & event == 1 ~ 2, # Negative
      TRUE ~ 0
    ), 
    event_men = case_when(
      diagage < meno_age_est & event == 1 ~ 1, #premenopausal
      diagage <= meno_age_est & event == 1 ~ 2, #postmenopausal 
      TRUE ~ 0
    ),
    bc_nolag = case_when( #only count events starting at study entry
      diagyear < as.numeric(matchyear) ~ 0, 
      TRUE ~ event
    ), 
    bc_2lag = case_when(#only count events starting 2 years after study entry
      diagyear < as.numeric(matchyear) +2 ~ 0, 
      TRUE ~ event
    ), 
    bc_5lag = case_when(#only count events starting 5 years after study entry
      diagyear < as.numeric(matchyear) +5 ~ 0, 
      TRUE ~ event
    ), 
    bc_10lag = case_when(#only count events starting 10 years after study entry
      diagyear < as.numeric(matchyear) + 10 ~ 0, 
      TRUE ~ event
    )
  )


# Format TCode for ease of join
clean_cohort <- clean_cohort %>% 
  mutate(
    TCode = as.character(TCode), 
    TCode = str_trim(TCode)
  )


# Clean the data 
covariates <- clean_cohort %>% 
  mutate(
    
    matchyear_cat = case_when(
      matchyear =="2004" ~ 1, 
      matchyear =="2005" ~ 2, 
      matchyear =="2006" ~ 3, 
      matchyear =="2007" ~ 4, 
      matchyear =="2008" ~ 5, 
      matchyear %in% c("2009","2010","2011","2012", "2013", "2014", "2015", "2016", "2017", "2018") ~ 6, 
      TRUE ~ 7
    ),
    matchyear_cat = factor(matchyear_cat, 
                           levels = 1:7, 
                           labels = c("2004", "2005", "2006", "2007", "2008", "2009-2018", "Unknown")
                           
    ),
    # Define plausible limit
    BMI = case_when(
      as.numeric(bodysizebminow) > 900 ~ NA_real_, 
      TRUE ~ as.numeric(bodysizebminow)
    ), 
    # Define the categories 
    BMI_cat = case_when( 
      BMI < 25 ~ 1,
      #BMI >= 18.5 & BMI < 25 ~ 1,
      BMI >= 25 & BMI < 30 ~ 2,
      BMI >= 30  ~ 3,
      TRUE ~ 4
    ), 
    #Define as a factor with labels
    BMI_cat = factor(BMI_cat, 
                     levels = 1:4,
                     labels = c("Underweight/Normal", "Overweight", "Obese", "Unkown")),
    
    #Define plausible limits
    Ethnicity = case_when(
      as.numeric(ethnew5) > 4 ~ NA_real_, 
      TRUE ~ as.numeric(ethnew5)
    ), 
    #Define as a factor with labels
    Ethnicity_cat = factor(Ethnicity, 
                           levels = 1:4,
                           labels = c("White", "Black", "South Asian", "Other/Mixed")),
    
    # Define categories 
    Carstairs_cat = case_when(
      is.na(car_dec) ~ 11,
      TRUE ~ car_dec
    ),
    Carstairs_cat = factor(Carstairs_cat, 
                           levels = 1:11, 
                           labels = c("1 (most)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (least)", "Unknown")), 
    
    # Define plausible limit
    Menarche = case_when(  
      as.numeric(menarcheage) > 20 ~ NA_real_, 
      as.numeric(menarcheage) < 8 ~ NA_real_, 
      TRUE ~ as.numeric(menarcheage)
    ), 
    #Define Categories
    Menarche_cat = case_when( 
      Menarche <=  11 ~0, 
      Menarche > 11 & Menarche <14 ~1,
      Menarche>=14 ~2, 
      TRUE ~ 3
    ), 
    #Define as a factor with labels
    Menarche_cat = factor(Menarche_cat, 
                          levels = 0:3,
                          labels = c("8-11", "12-14", "14-20", "Unknown")),
    
    # ER status 
    ER = case_when(
      event == 1 & er_Status == "Negative" ~ 2, 
      event == 1 & er_Status == "Positive" ~ 1, 
      event == 1 & er_Status == "Unknown" ~ 3, 
      TRUE ~ 0
    ),
    ER_cat = factor(ER, 
                    levels = 0:3, 
                    labels = c("Nonevent", "Positive","Negative", "Unknown")
                    
    ),
    
    # Menopause at diagnosis
    diag_men = case_when(
      event == 1 & diagage >= meno_age_est ~ 2, 
      event == 1 & diagage < meno_age_est ~ 1, 
      TRUE ~ 0
    ),
    diag_men_cat = factor(diag_men, 
                          levels = 0:2, 
                          labels = c("Nonevent", "premenopausal", "postmenopausal")
                          
    ),
    
    # Define plausible limit
    HRT = case_when(
      as.numeric(hrtever) > 2 ~ 2, 
      is.na(hrtever) ~ 2,
      TRUE ~ as.numeric(hrtever)
    ), 
    #Define as a factor with labels
    HRT_cat = factor(HRT, 
                     levels = 0:2, 
                     labels = c("No", "Yes", "Unknown")), 
    
    # Define plausible limit
    Alcohol = case_when(
      as.numeric(R1alcoholstatus) > 2 ~ 3,
      is.na(R1alcoholstatus) ~3, 
      TRUE ~ as.numeric(R1alcoholstatus)
    ), 
    #Define as a factor with labels
    Alcohol_cat = factor(Alcohol,
                         levels = 0:3, 
                         labels = c("Never", "Current", "Past", "Unknown")
    ), 
    
    # Define plausible limit
    Smoking = case_when(
      as.numeric(R1smokingstatus) > 2 ~ 3, 
      is.na(R1smokingstatus) ~ 3,
      TRUE ~ as.numeric(R1smokingstatus)
    ), 
    #Define as a factor with labels
    Smoking_cat = factor(Smoking, 
                         levels = 0:3, 
                         labels = c("Never", "Former", "Current", "Unknown")),
    
    # Define plausible limit
    Fam_Hist = case_when(
      as.numeric(fambrca) > 2 ~ 3, 
      TRUE ~ as.numeric(fambrca)
    ), 
    #Define as a factor with labels
    Fam_Hist_cat = factor(Fam_Hist, 
                          levels = 0:2, 
                          labels = c("No", "Yes", "Unknown")), 
    
    # Define plausible limit
    Menopause = case_when(
      as.numeric(R1menopause) >= 8 ~ NA_real_,
      as.numeric(R1menopause) == 1 ~ 1,
      as.numeric(R1menopause) == 3 ~ 1,
      as.numeric(R1menopause) == 2 ~ 0,
      as.numeric(R1menopause) == 4 ~ 0,
      TRUE ~ as.numeric(R1menopause)
    ), 
    #Define as a factor with labels
    Menopause_cat = factor(Menopause, 
                           levels = 0:1, 
                           labels = c("Premenopausal", "Postmenopausal")),
    
    # Menopause age
    Men_broad = case_when(
      is.na(R1menopause) | R1menopause %in% c(8, 10) ~ NA, # not know or questionnaire not completed
      TRUE ~ R1menopause),
    # verify age checks 
    Menopause_age = case_when(
      Men_broad %in% c(1, 3)  & x_age_birth_last < 700 & x_age_birth_last < as.numeric(meno_age_est) & !is.na(meno_age_est) & as.numeric(meno_age_est) != 999 ~ as.numeric(meno_age_est), # age last birth known and before meno_age_est
      Men_broad %in% c(1, 3) & x_age_birth_last %in% c(777,888) & !is.na(meno_age_est) & as.numeric(meno_age_est) != 999 ~ as.numeric(meno_age_est), # age last birth not known/NA so assume age is valid
      Men_broad %in% c(1, 3) ~ 999, # Postmeno, but due to conditions above, not able to calculate the age
      Men_broad %in% c(9, 888) ~ 888, # Menopausal status not known - could use a different error code? Different levels of unknown for this variable 
      TRUE ~ 777), # Pre menopausal 
    # Define categories 
    Menopause_age_cat = case_when(
      Menopause_age == 777 ~ 0,
      Menopause_age < 50 ~ 1,
      Menopause_age >= 50 & Menopause_age < 55 ~ 2,
      Menopause_age >= 55 & Menopause_age < 700 ~ 3,
      Menopause_age %in% c(888, 999) ~ NA_real_,
      Menopause_age < AgeatEntry ~ NA_real_,
      TRUE ~ NA_real_), 
    #Define as a factor with labels
    Menopause_age_cat = factor(Menopause_age_cat, 
                               levels = c(0:3), 
                               labels = c("Premenopausal", "<50", "50-54", "55+")), 
    
    # Define a plausible limit 
    Parity = case_when(
      x_parity == 0 ~ 0, 
      x_parity %in% c(1,2)~ 1, 
      x_parity >= 3 & x_parity <15 ~ 2, 
      TRUE ~ 3
    ), 
    Parity_cat = factor(Parity,
                        levels = 0:3,
                        labels = c("0", "1-2", "3+", "Unknown")
                        
    ), 
    
    # Age at First Birth
    # Define feasible limits 
    Birth1_age = case_when(
      x_age_birth_1 == 888 ~ 4, 
      x_age_birth_1 == 777 ~ 0,
      x_age_birth_1 < 25 ~ 1, 
      x_age_birth_1 >=25 & x_age_birth_1 < 30 ~2, 
      x_age_birth_1 >= 30 ~ 3,
      TRUE ~ 4
    ), 
    Birth1_age_cat = factor(Birth1_age,
                            levels = 0:4,
                            labels = c("Never", "<25", "25-30", "30+", "Unknown")
                            
    ),
    
    # Age at Entry
    AgeEntry_cat = case_when(
      AgeatEntry < 50~0, 
      AgeatEntry >= 50~1, 
      TRUE ~ NA_real_
    ), 
    AgeEntry_cat = factor(AgeEntry_cat,
                          levels = 0:1,
                          labels = c("16-49", "50+")), 
    
    # Age at Entry small categories
    AgeEntry_smallcat = case_when(
      AgeatEntry < 25~ 0, 
      AgeatEntry >= 25 & AgeatEntry <30 ~1,
      AgeatEntry >= 30 & AgeatEntry <35 ~2,
      AgeatEntry >= 35 & AgeatEntry <40 ~3,
      AgeatEntry >= 40 & AgeatEntry <45 ~4,
      AgeatEntry >= 45 & AgeatEntry <50 ~5,
      AgeatEntry >= 50 & AgeatEntry <55 ~6,
      AgeatEntry >= 55 & AgeatEntry <60 ~7,
      AgeatEntry >= 60 & AgeatEntry <65 ~8,
      AgeatEntry >= 65 & AgeatEntry <70 ~9,
      AgeatEntry >= 70 ~10,
      TRUE ~ NA_real_
    ), 
    AgeEntry_smallcat = factor(AgeEntry_smallcat,
                               levels = 0:10,
                               labels = c("<25","25-30", "30-35","35-40", "40-45","45-50", "50-55","55-60", "60-65","65-70", "70+")),
    
    # Education 
    Educ_cat = case_when(
      educage <=18 ~ 0, 
      educage > 18 & educage <100 ~ 1, 
      educage >= 100 ~ 2, 
      TRUE ~ 2
    ), 
    Educ_cat = factor(Educ_cat, 
                      levels = 0:2, 
                      labels = c ("Up to high school", "More than high school", "Unknown"))
  )

# Join the data with the air pollution risk factors 
complete_cohort <- covariates %>% 
  mutate(
    p_days365 = c_age365 - age_start365,
    p_years = p_days365 / 365
  )


#Filtering complete_cohort based on the menopausal_status determined in the .qmd file: 
if(menopausal_status == "post"){
  complete_cohort <- complete_cohort %>% 
    filter(
      Menopause_cat == "Postmenopausal"
    )
  print("This analysis is on POSTMENOPAUSAL WOMEN ONLY")
}else if(menopausal_status == "pre"){
  complete_cohort <- complete_cohort %>% 
    filter(
      Menopause_cat == "Premenopausal"
    )
  print("This analysis is on PREMENOPAUSAL WOMEN ONLY")
}else if(menopausal_status == "all"){
  print("This analysis is for all women in the study")
} else {
  stop("INVALID MENOPAUSE SPECIFICATION: options are 'pre', 'post', or 'all'")
}

