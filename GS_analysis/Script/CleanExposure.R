# Cleaning the exposure data

cleanexposure <- function(ap_data, cohort_data){
# Exposure initial formatting 
long_pollution <- ap_data %>%
  # Pivot longer for all of the columns that begin with NO2, P25, P10 and put the exposure values in the second column
  pivot_longer(
    cols = matches("^(NO2|P25|P10)B25_\\d+$"),
    names_to = "colname",
    values_to = "value"
  ) %>%
  mutate(
    # Pull the overall exposure name (NO2, P25, P10), and the exposure year and clean the data
    pollutant = str_extract(colname, "^(NO2|P25|P10)"),
    suffix = str_sub(colname, -2)
  ) %>% 
  # Create a compound variable 
  mutate(
    pollutant = paste(pollutant, suffix, sep = "_")
  ) %>%
  select(TCode, pollutant, value)

# Pivot back wider to create the full data set
airpollution <- long_pollution %>% 
  pivot_wider(
    names_from = pollutant,
    values_from = value
  ) 

# Join the air pollution data with the risk factor data 
all_data <- cohort_data %>%
  left_join(airpollution, by = "TCode") 

# Relong the data
long_data <- all_data %>%
  pivot_longer(
    cols = matches("^(NO2|P25|P10)_\\d{2}$"),   # two-digit years
    names_to = c(".value", "year"),
    names_pattern = "(NO2|P25|P10)_(\\d{2})"
  ) %>%
  mutate(
    year = as.numeric(paste0("20", year))       # convert 04 → 2004
  ) %>%
  arrange(TCode, year)


long_data <- long_data %>%
  mutate(
    # Format Exposure data
    P25 = as.numeric(P25), 
    P10 = as.numeric(P10), 
    NO2 = as.numeric(NO2)
  ) %>% mutate(
    # calculate the years between start and end to follow up
    difference = as.numeric(date_fup_end - date_fup_start) / 365,
    # round up to max amount of years of follow up
    difference = ceiling(difference),
    # calculate the last calendar year of follow up by adding the match end to the entry year
    matchend = as.numeric(matchyear) + as.numeric(difference)
  ) 

long_data <- long_data %>%
  mutate(
    included = case_when(
      year < matchyear ~ 0, 
      year > matchend ~ 0,
      TRUE ~ 1
    ), 
    interpolated = case_when(
      is.na(NO2) ~1, 
      TRUE ~ 0
    )
  ) 


#Function for filling in missing data
fill_midpoint_gaps <- function(x) {
  n <- length(x)
  
  # Step 1: Identify first and last non-NA
  first_non_na <- which(!is.na(x))[1]
  last_non_na  <- tail(which(!is.na(x)), 1)
  
  # If no non-NA or only one non-NA, fallback to LOCF/NOCB everywhere
  if (is.na(first_non_na) || first_non_na == last_non_na) {
    return(zoo::na.locf(zoo::na.locf(x, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE))
  }
  
  # Step 2: Fill edges ONLY
  # left edge
  if (first_non_na > 1) {
    x[1:(first_non_na-1)] <- x[first_non_na]
  }
  # right edge
  if (last_non_na < n) {
    x[(last_non_na+1):n] <- x[last_non_na]
  }
  
  # Step 3: Fill interior gaps with midpoint
  # Work on the original values (after edge filling)
  original <- x
  
  na_runs <- rle(is.na(original))
  ends <- cumsum(na_runs$lengths)
  
  start <- 1
  for (i in seq_along(na_runs$lengths)) {
    end <- ends[i]
    
    if (na_runs$values[i]) {  # this is an NA run
      
      left <- start - 1
      right <- end + 1
      
      if (left >= 1 && right <= n && 
          !is.na(original[left]) && !is.na(original[right])) {
        
        mid <- (original[left] + original[right]) / 2
        x[start:end] <- mid
      }
    }
    
    start <- end + 1
  }
  
  return(x)
}

# Linear Interpolation of air pollution data
interpolated_long_data <- long_data %>%
  group_by(TCode) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    NO2 = fill_midpoint_gaps(NO2),
    P25 = fill_midpoint_gaps(P25),
    P10 = fill_midpoint_gaps(P10)
  ) %>%
  ungroup() %>%
  filter(included == 1)

# Clean data for point/average calculation  
long_data_clean <- long_data %>%
  filter(
    included == 1
  ) 

# Creating the cleaned year by year dataset
airpollution_clean <- interpolated_long_data %>%
  dplyr::select(
    TCode, year, NO2, P25, P10
  ) %>%
  # build the suffix
  mutate(
    suffix = str_sub(year, -2)
  ) %>%
  #create the column names
  mutate(
    NO2_col = paste0("NO2_", suffix),
    P25_col = paste0("P25_", suffix),
    P10_col = paste0("P10_", suffix)
  ) %>%
  #pivot back out
  pivot_wider(
    id_cols = TCode,       
    names_from = suffix,
    values_from = c(NO2, P25, P10),
    names_glue = "{.value}_{suffix}"
  )


# SINGULAR POINT ESTIMATE
snglpoint_pollution <- long_data_clean %>%
  group_by(TCode) %>%
  # sort by year within each TCode
  arrange(year, .by_group = TRUE) %>%        
  summarise(
    # first non-NA of each exposure by year
    pointNO2 = first(na.omit(NO2)), 
    pointP25 = first(na.omit(P25)), 
    pointP10 = first(na.omit(P10)), 
    .groups = "drop"
  ) %>%
  select(TCode, pointNO2, pointP25, pointP10) %>%
  mutate(
    pointNO2 = as.numeric(pointNO2), 
    pointP25 = as.numeric(pointP25), 
    pointP10 = as.numeric(pointP10)
  )

# AVERAGE
avg_pollution <- long_data_clean %>%
  group_by(TCode) %>%
  summarise(
    # Average exposures including NA years 
    avgNO2 = mean(NO2, na.rm = TRUE),
    avgP25 = mean(P25, na.rm = TRUE),
    avgP10 = mean(P10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(TCode, avgNO2, avgP25, avgP10) %>%
  mutate(
    avgNO2 = as.numeric(avgNO2), 
    avgP25 = as.numeric(avgP25), 
    avgP10 = as.numeric(avgP10)
  )

# Final dataset with average and point values 
complete_cohort <- cohort_data %>% 
  left_join(snglpoint_pollution, by = "TCode") %>% 
  left_join(avg_pollution, by = "TCode") %>%
  left_join(airpollution_clean, by = "TCode") 

return(complete_cohort)

}
