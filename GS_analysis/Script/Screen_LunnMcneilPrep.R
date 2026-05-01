complete_cohort_screen <- complete_cohort %>%
  left_join(data_england, by = "TCode") %>%
  filter(
    country == "England"
  ) %>%
  mutate(
    dob = as.Date(date_birth),
    dob_adj = if_else(month(dob) == 2 & day(dob) == 29, dob - 1, dob),  # this deals with the leap year
    start_fu = as.Date(date_fup_start),
    end_fu = as.Date(date_fup_end),
    
    birthday_50 = dob_adj + years(50),
    birthday_71 = dob_adj + years(71),
    
    SCREEN_start_fup = pmax(start_fu, birthday_50, na.rm = TRUE),
    SCREEN_end_fup   = pmin(end_fu, birthday_71, na.rm = TRUE), 
    
    SCREEN_start_age365 = as.numeric(SCREEN_start_fup - dob),
    SCREEN_end_age365   = as.numeric(SCREEN_end_fup - dob), 
  ) %>%
  filter(
    SCREEN_end_age365 > SCREEN_start_age365
  )

cohort_mensplit_screen <- complete_cohort_screen %>% 
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
cohort_mensplit_screen <- tmerge(data1 = cohort_mensplit_screen, data2 = cohort_mensplit_screen,
                          id = TCode, tstart = SCREEN_start_age365, tstop = SCREEN_end_age365,
                          outcome = event(c_age365, event))

# Splitting the cohort by menopause
times <- cohort_mensplit_screen[, c("TCode", "age_meno365")]
cohort_mensplit_screen <- tmerge(data1 = cohort_mensplit_screen, data2 = times,
                          id = TCode, menoband = tdc(age_meno365))

# Adding menopausal labels
meno_labels <- c("premenopausal", "postmenopausal")
cohort_mensplit_screen$menobandf <- factor(cohort_mensplit_screen$menoband,
                                    levels = 0:1,
                                    labels = meno_labels)

# Adjusting follow up time for each row
cohort_mensplit_screen <- cohort_mensplit_screen %>%
  mutate(
    interval_days = tstop - tstart,
    p_years = interval_days / 365, 
    followup_year = floor((tstart - age_start365) / 365), 
    screen = factor(screen_detected,
                    levels = c(0, 1, 2, 3, 4),
                    labels = c("Nonevent", "Screen Detected", "Interval Cancer", "Other", "Unknown"))
  )


# Screen detected status
yesscreen <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Screen Detected" ~ 1, 
      TRUE ~ 0
    ),
    type = "screen"
  )

noscreen <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Interval Cancer" ~ 1, 
      TRUE ~ 0
    ),
    type = "interval"
  )

other <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Other" ~ 1, 
      TRUE ~ 0
    ),
    type = "other"
  )

unknown <- cohort_mensplit_screen %>%
  mutate(
    bc_nolag = case_when(
      outcome == 1 & screen == "Unknown" ~ 1, 
      TRUE ~ 0
    ),
    type = "unk"
  )
screened <- rbind(yesscreen, noscreen, other, unknown)
screened <- screened %>%
  mutate(
    type = factor(type, levels = c("screen", "interval", "other", "unk")),
    avgNO2 = avgNO2/10,
    avgP25 = avgP25/5, 
    avgP10 = avgP10/5
  )

