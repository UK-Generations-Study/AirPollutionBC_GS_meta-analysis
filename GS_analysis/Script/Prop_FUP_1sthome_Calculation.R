# calculating the % of PY at first address

peoplewhomoved <- data_gs %>%
  arrange(TCode, UpdatedWhen) %>%   # optional: sort dates
  group_by(TCode) %>%
  mutate(update_id = row_number()) %>%  # create index per TCode
  ungroup() %>%
  pivot_wider(
    id_cols = TCode,
    names_from = update_id,
    values_from = UpdatedWhen,
    names_prefix = "UpdatedWhen_"
  ) %>%
  select(
    TCode, UpdatedWhen_2, UpdatedWhen_3
  ) %>%
  mutate(
    move1_year = as.integer(format(UpdatedWhen_2, "%Y")), 
    move2_year = as.integer(format(UpdatedWhen_3, "%Y")), 
    move = case_when(
      move1_year == 1980 ~ move2_year, 
      TRUE ~ move1_year
    )
    ) %>%
  left_join(data_realyears, by = "TCode") %>%
  select(
    TCode, move, Entry
  ) %>%
  mutate(
    fup_firstmove = (as.numeric(move) - as.numeric(Entry)), 
    fup_firstmove = case_when(
      fup_firstmove <0 ~ 0, 
      TRUE ~ fup_firstmove
    )
  )
fup_est_pre1stmove <- sum(peoplewhomoved$fup_firstmove, na.rm = T)

nomove_fupest <- clean_nomove_GS %>%
  left_join(complete_cohort, by = "TCode") 

fup_nomove <- sum(nomove_fupest$followup_years_age, na.rm=T)

prop_fup_firstaddress <- (fup_est_pre1stmove + fup_nomove) / sum(complete_cohort$followup_years_age, na.rm = T)

prop_potentialmisclassification <- 1 - prop_fup_firstaddress

# 0.216632408
