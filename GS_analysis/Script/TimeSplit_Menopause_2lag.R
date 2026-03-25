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

# Check time series correctly 
cohort_mensplit %>%
  group_by(menobandf) %>%
  summarise(
    intervals = n(),
    BC = sum(outcome),
    PY = sum(p_years) )

cohort_mensplit %>%
  summarise(
    intervals = n(),
    BC = sum(outcome),
    PY = sum(p_years) )

cohort_mensplit_2lag <- cohort_mensplit

