# This is to build the GS cohort for air pollution cox regression analysis 
# Prepare the risk factor data 
riskfactors <- load_RDS_data("v20230927", "RDSRiskFactors.csv",
                             excl_date = "2025-04-01",
                             cancel_date = "2025-05-01")

# Remove records with missing TCode
riskfactors <- riskfactors %>%
  filter(!is.na(TCode), TCode != "NULL", TCode != "")

# Check for duplicates
riskfactors <- riskfactors %>%
  arrange(TCode) %>%
  group_by(TCode) %>%
  mutate(count_TCode = order(TCode)) %>%
  ungroup()

# Drop duplicates
riskfactors <- riskfactors %>%
  filter(riskfactors$count_TCode == 1)

# Drop records without questionnaire
riskfactors <- riskfactors %>%
 filter(!is.na(AgeatEntry), AgeatEntry != "NULL")

# convert datetime to date format
riskfactors <- riskfactors %>%
  mutate(
    date_birth = as.Date(ADOB_F),
    date_entry = as.Date(EntryDate_F),
    date_fup_start = as.Date(fupca_start_F),
    date_fup_end = as.Date(fupca_end_F),
    date_r2 = as.Date(R2Date_F),
    date_r3 = as.Date(R3Date_F),
    date_r4 = as.Date(R4Date_F),
    # ! add one day to follow-up when exit on day of entry ?
    age_start365 = duration_no_leapday(date_birth, date_fup_start),
    age_end365 = duration_no_leapday(date_birth, date_fup_end),
    start_time365 = 0,
    end_time365 = duration_no_leapday(date_fup_start, date_fup_end),
    
    age_entry_365 = duration_no_leapday(date_birth, date_entry),
    age_r2_365 = duration_no_leapday(date_birth, date_r2),
    age_r3_365 = duration_no_leapday(date_birth, date_r3),
    age_r4_365 = duration_no_leapday(date_birth, date_r4)
  )

# Load cancer summary data
casummary <- read_csv("R:\\Shared\\GENERATIONS\\CoreData\\v20250327\\RDSCaSummary_V1_1.csv")
casummary <- apply_exclusions(casummary,excl_date,cancel_date)

# Remove records where TCode is missing
casummary <- casummary %>% 
  filter(!is.na(TCode), TCode != "NULL", TCode != "")

# Remove records where ICD code is missing
casummary <- casummary %>%
  filter(!is.na(ICDt), ICDt != "NULL", ICDt != "")

# Merge date of birth and start of follow-up to CAsummary for age calculation
casummary <- merge(x = casummary,
                   y = riskfactors[,
                                   c("TCode", "ADOB_F", "fupca_start_F")],
                   by.x = c("TCode"), by.y = c("TCode"), all.x = TRUE)

# Add more high def screening variable
newcancer<- data_newcancer %>%
  select(
  TCode, ICD_CODE, CANCER_SITE, SCREENINGSTATUSCOSD_CODE, DIAGNOSIS_DATE_SHIFTED
  ) %>%
  mutate(
    DIAGNOSIS_DATE_SHIFTED = as.Date(DIAGNOSIS_DATE_SHIFTED)
  )
og_data <- casummary %>%
  mutate(
    diagdate_f = as.Date(diagdate_f, format = "%d/%m/%Y"),
    DIAGNOSIS_DATE_SHIFTED = as.Date(diagdate_f)
  )
casummary <- og_data %>%
  left_join(newcancer, by = c("TCode", "DIAGNOSIS_DATE_SHIFTED"))

# identify outcome event(s)
casummary <- casummary %>%
  mutate(
    # Recategorize so the date is formatted correctly
    diagdate_f = as.Date(diagdate_f, format = "%d/%m/%Y"),

    # Create new variables and recode position 0 to 999
    # so 'no registry' or 'no reported' info is last
    reginfo_clusterino99 = ifelse(Reginfo_Clusterino == 0, 999,
                                  Reginfo_Clusterino),
    report_cluster99 = ifelse(Report_Cluster == 0, 999, Report_Cluster),
    report_group99 = ifelse(Report_GroupDateSite == 0, 999, Report_GroupDateSite),
    # Convert datetime to date format and calculate age
    date_birth_c = as.Date(ADOB_F, format = "%Y-%m-%d %H:%M:%S"),
    date_fup_start_c = as.Date(fupca_start_F, format = "%Y-%m-%d %H:%M:%S"),
    date_diag = as.Date(diagdate_f, format = "%Y-%m-%d %H:%M:%S"),
    diagage365 = duration_no_leapday(date_birth_c, date_diag),
    time_to_dx365 = duration_no_leapday(date_fup_start_c, date_diag),
    #Invasive cancer (including DCIS-breast but excluding NMSC)
    cancer = if_else(
      !((str_sub(ICDt, 1, 3) == "C44") |
          (str_sub(ICDt, 1, 3) == "173")) & #excluding non melanoma skin cancer
        ((str_sub(ICDt, 1, 1) == "C") |
           (str_sub(ICDt, 1, 1) == "1") |
           (str_sub(ICDt, 1, 2) == "20") |
           (str_sub(ICDt, 1, 4) == "D051") |
           (str_sub(ICDt, 1, 4) == "D059") |
           (str_sub(ICDt, 1, 4) == "D05Z") |
           (str_sub(ICDt, 1, 4) == "2330")), 1, 0),
    # Invasive or DCIS-breast cancer
    breast_cancer = if_else(
      (str_sub(ICDt, 1, 3) == "C50") |
        (str_sub(ICDt, 1, 3) == "174") |
        (str_sub(ICDt, 1, 4) == "D051") |
        (str_sub(ICDt, 1, 4) == "D059") |
        (str_sub(ICDt, 1, 4) == "D05Z") |
        (str_sub(ICDt, 1, 4) == "2330"), 1, 0),
    # Invasive breast cancer only
    breast_cancer_invasive = if_else(
      (str_sub(ICDt, 1, 3) == "C50") |
        (str_sub(ICDt, 1, 3) == "174"), 1, 0),
    # DCIS-breast cancer only
    breast_cancer_dcis = if_else(
      (str_sub(ICDt, 1, 4) == "D051") |
        (str_sub(ICDt, 1, 4) == "D059") |
        (str_sub(ICDt, 1, 4) == "D05Z") |
        (str_sub(ICDt, 1, 4) == "2330"), 1, 0),
    # Ovarian cancer and fallopian tube
    ovarian_cancer = if_else(
      (str_sub(ICDt, 1, 3) == "C56") |
        (str_sub(ICDt, 1, 4) == "C570") |
        (str_sub(ICDt, 1, 4) == "1830") |
        (str_sub(ICDt, 1, 4) == "1832"), 1, 0),
    # Colo-rectal cancer
    colorectal_cancer = if_else(
      (str_sub(ICDt, 1, 3) == "C18") |
        (str_sub(ICDt, 1, 3) == "C19") |
        (str_sub(ICDt, 1, 3) == "C20") |
        (str_sub(ICDt, 1, 3) == "153") |
        (str_sub(ICDt, 1, 3) == "154"), 1, 0),
    # Pancreatic cancer
    pancreatic_cancer = if_else(
      (str_sub(ICDt, 1, 3) == "C25"), 1, 0),
    # Trachea, bronchus and lung cancer
    lung_cancer = if_else(
      (str_sub(ICDt, 1, 3) == "162") |
        (str_sub(ICDt, 1, 3) == "C33") |
        (str_sub(ICDt, 1, 3) == "C34"), 1, 0),
    # breast cancer specific recoding
    # recode ER NA to text 'Unknown'
    er_Status = case_when(
      breast_cancer == 1 &
        er_Status == "Positive" ~ "Positive",
      breast_cancer == 1 &
        er_Status == "Negative" ~ "Negative",
      breast_cancer == 1 ~ "Unknown",
      TRUE ~ NA_character_), 
    # Screen Detected 
    screen_detected = case_when(
      breast_cancer == 1 &
        screen_Detected == "Y" ~ "Yes",
      breast_cancer == 1 &
        screen_Detected == "N" ~ "No",
      breast_cancer == 1 &
        screen_Detected == "U" ~ "Unknown",
      breast_cancer == 1 ~ "Unknown",
      TRUE ~ NA_character_)
  )

# first ever cancer
  # Select first cancer (per woman)
  cancer <- casummary %>%
    filter(cancer == 1) %>%
    arrange(TCode, diagdate_f,reginfo_clusterino99, report_cluster99, report_group99) %>%
    group_by(TCode) %>%
    mutate(n_tcode = order(TCode)) %>%
    filter(n_tcode == 1) %>%
    ungroup()
  summary_table <- data.frame(
    "Category" = c("All cancer (including DCIS, excluding NMSC)",
                   "DCIS or invasive breast",
                   "Invasive breast",
                   "DCIS breast",
                   "Ovarian and fallopian tube",
                   "Colo-rectal",
                   "Pancreas",
                   "Trachea, bronchus and lung cancer"
    ),
    "Count" = c(
      sum(cancer$cancer),
      sum(cancer$breast_cancer),
      sum(cancer$breast_cancer_invasive),
      sum(cancer$breast_cancer_dcis),
      sum(cancer$ovarian_cancer),
      sum(cancer$colorectal_cancer),
      sum(cancer$pancreatic_cancer),
      sum(cancer$lung_cancer)
    )
  )
  print(kable(summary_table,
              format = "markdown",
              col.names = c("Cancer", "Cases"),
              caption = paste("Number of women with a cancer where this cancer",
                              "is first ever cancer of any type",
                              "(excluding NMSC)",
                              "\n(Includes prevalent and incident cancers)")))


# Merge risk factor data and cancer data
rf0 <- merge(x = riskfactors,
             y = cancer,
             by.x = c("TCode"),
             by.y = c("TCode"),
             all.x = TRUE)

# set cohort event to selected cancer or zero for rest of cohort
rf0 <- rf0 %>%
  mutate(selected_event = ifelse(is.na(!!sym(select_cancer)),
                                 0,    !!sym(select_cancer)))


# Censoring dates based on selected outcome 

# Prepare dataset for cohort analysis (censoring age)
# ! cox_event is set in earlier code based on your definition of cancer event
rf0 <- rf0 %>%
  mutate(
    # create outcome event if during follow-up time
    event = ifelse(
      (selected_event == 1) &                 # event actually selected
        !is.na(diagage365) &                    # diagnosis age exists
        (diagage365 <= age_end365) &            # event before end of follow-up
        (diagage365 > age_start365),            # event after start of follow-up
      1,                                      # → event occurred during interval
      0                                       # → otherwise, no event
    ),
    # event_surv based on survival times / event_age based on ages
    # event_surv and event_age are identical
    # but illustrate analyses are similar
    event_surv = ifelse(
      (selected_event == 1) &
        !is.na(time_to_dx365) &
        (time_to_dx365 <= end_time365) &
        (time_to_dx365 > start_time365),  1, 0),
    event_age = ifelse(
      (selected_event == 1) &
        !is.na(diagage365) &
        (diagage365 <= age_end365) &
        (diagage365 > age_start365),  1, 0),
    # create censoring age to event_age as minimum of end of
    # follow-up age or diagnosis age for any invasive cancer or breast-DCIS
    # create survival age to event_time as minimum of end of follow-up time or
    # diagnosis time for any invasive cancer or breast-DCIS
    c_age365 = pmin(age_end365, diagage365, na.rm = TRUE),
    survival_time = pmin(end_time365, time_to_dx365, na.rm = TRUE),
    # calculate person-years of follow-up
    followup_years_age = ifelse(
      c_age365 >= age_start365,
      (c_age365 - age_start365) / 365, NA),
    followup_years_surv = ifelse(
      survival_time >= start_time365,
      (survival_time - start_time365) / 365, NA)
  )

# subset to data with valid follow-up time
print(paste("Number of records in riskfactor and cancer merged dataset:",
            nrow(rf0)))
base <- rf0 %>% filter(!is.na(followup_years_age))
base <- base %>% filter(followup_years_age > 0)
print(paste("Number of records with valid follow-up",
            "(base dataset for cohort analysis):",
            nrow(base)))
print(paste("Distribution of follow-up time (years)"))
print(summary(base$followup_years_age))
print(paste("Number of women with selected outcome event in analytic cohort ",
            "(i.e. inside cohort window): ",
            sum(base$event)))














