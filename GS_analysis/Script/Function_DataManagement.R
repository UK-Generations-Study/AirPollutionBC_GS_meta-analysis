################################################################################
#' CasesPY.R
#' MEJones
#' April 2024
#'
#' This tabulates events and person-years
#' @param DATA dataset for basic/simple cohort analysis with outcome defined
#' @param STRATA variable that specifies the strata
#' @param OUTCOME variable for the outcome event
#' @param PYEARS variable for the person-years
#' @export
#' @examples

casespy <- function(DATA, STRATA, OUTCOME, PYEARS) {
  require(dplyr)
  require(rlang)
  
  # Convert variable names to symbols
  strata_sym <- ensym(STRATA)
  outcome_sym <- ensym(OUTCOME)
  pyears_sym <- ensym(PYEARS)
  
  # Get the name of the STRATA variable as a string
  strata_chr <- as_string(strata_sym)
  outcome_chr <- as_string(outcome_sym)
  covariate_name <- str_remove(strata_chr, "_cat$")
  
  # Calculate the summary statistics by group
  result <- DATA %>%
    group_by(!!strata_sym) %>%
    summarise(
      N = n(),
      cases = sum(!!outcome_sym),
      personyears = sum(!!pyears_sym),
      mean_NO2 = mean(pointNO2, na.rm = TRUE),
      mean_PM2.5 = mean(pointP25, na.rm = TRUE),
      mean_PM10 = mean(pointP10, na.rm = TRUE),
      .groups = "drop"
    )  %>%
    arrange(!!strata_sym) %>%
    mutate(
      covariate = covariate_name,
      personyears = round(personyears, 2)
    ) %>%
    rename(label = !!strata_sym) %>%
    mutate(rate_per_100k = round(cases / personyears * 100000, 2)) %>%
    dplyr::select(covariate, label, N, cases, personyears, rate_per_100k, mean_NO2, mean_PM2.5, mean_PM10)
  
  # Run ANOVA tests for pollutants
  aov_NO2   <- summary(aov(pointNO2 ~ get(strata_chr), data = DATA))
  p_anova_NO2 <- aov_NO2[[1]][["Pr(>F)"]][1]
  
  aov_PM25  <- summary(aov(pointP25 ~ get(strata_chr), data = DATA))
  p_anova_PM25 <- aov_PM25[[1]][["Pr(>F)"]][1]
  
  aov_PM10  <- summary(aov(pointP10 ~ get(strata_chr), data = DATA))
  p_anova_PM10 <- aov_PM10[[1]][["Pr(>F)"]][1]
  
  # Chi-square test for outcome
  chisq_outcome <- chisq.test(table(DATA[[strata_chr]], DATA[[outcome_chr]]))
  p_chisq_bc5lag <- chisq_outcome$p.value
  
  # Chi-square test for outcome
  chisq_outcome <- chisq.test(table(DATA[[strata_chr]], complete_cohort$bc_nolag))
  p_chisq_bcnolag <- chisq_outcome$p.value
  
  # Remove label attribute if it exists
  attr(result[["label"]], "label") <- NULL
  
  # Calculate the rate per 100k
  result <- result %>%
    mutate(rate_per_100k = cases / personyears * 100000, 
           rate_per_100k = round(rate_per_100k, 2), 
           p_anova_NO2 = round(p_anova_NO2, 3), 
           p_anova_PM25 = round(p_anova_PM25, 3), 
           p_anova_PM10 = round(p_anova_PM10, 3), 
           p_chisq_bc5lag = round(p_chisq_bc5lag, 3), 
           p_chisq_bcnolag = round(p_chisq_bcnolag, 3)) %>%
    dplyr::select(covariate, label, N, cases, personyears, rate_per_100k, mean_NO2,p_anova_NO2, mean_PM2.5,p_anova_PM25, mean_PM10, p_anova_PM10, p_chisq_bc5lag, p_chisq_bcnolag)
  
  return(result)
}
################################################################################
#' Days_no_leapday.R
#' MEJones
#' May 2024
#'
#' This function will calculate days from 1/1/1900 but skips counting leap-days
#' @param a_date a date variable
#' @import lubridate
#' @export
#' @examples

days_no_leapday <- function(a_date) {
  require(lubridate)
  ifelse(
    (yday(make_date(year = a_date, month = 12, day = 31)) == 366)
    & (yday(a_date) > 59) ,
    ((365 * (as.numeric(format(a_date, "%Y")) - 1960)
      + as.numeric(yday(a_date)  - 2))),
    ((365 * (as.numeric(format(a_date, "%Y")) - 1960)
      + as.numeric(yday(a_date)  - 1)))
  )
}

duration_no_leapday <- function(date1, date2) {
  days_no_leapday(date2) - days_no_leapday(date1)
}

# END OF Days_no_leapday.R
################################################################################
# code to check factors are assigned correctly
check_factors <- function(VAR, FACTOR, ID, DATA) {
  cat("==========", "\n")
  cat(" Variable:", deparse(substitute(VAR)), "\n")
  cat(" Factor:  ", deparse(substitute(FACTOR)), "\n")
  cat(" ID:      ", deparse(substitute(ID)), "\n")
  cat("----------", "\n")
  temp <- aggregate(VAR ~ FACTOR, DATA,
                    FUN = function(x) c(min = min(x), max = max(x)))
  temp$Count <- aggregate(ID ~ FACTOR,
                          data = DATA,
                          FUN = function(x) length(unique(x)))$ID
  print(temp)
}
################################################################################
