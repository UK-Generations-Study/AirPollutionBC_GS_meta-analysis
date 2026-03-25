# Plot Functions


# Point and Average full cohort plots 
full_cohort_plot <- function(data, lag_value, exp_def, plot_title = NULL) {
  
  # Filter + clean
  results_clean <- data %>% 
    filter(
      (Variable == "NO2" & Exposure == "per 10") |
        (Variable != "NO2" & Exposure == "per 5"),
      exp_definition == exp_def,
      Outcome == lag_value
    ) %>%
    mutate(
      HR = as.numeric(str_extract(`HR (95% CI)`, "^[0-9.]+")),
      CI_lower = as.numeric(str_extract(`HR (95% CI)`, "(?<=\\().+?(?=–)")),
      CI_upper = as.numeric(str_extract(`HR (95% CI)`, "(?<=–).+?(?=\\))")),
      Model = factor(Model, levels = c("C", "B", "A")),
      Variable = factor(Variable, levels = c("NO2", "PM2.5", "PM10"))
    )
  
  # Default title if not supplied
  if (is.null(plot_title)) {
    plot_title <- paste("Estimate with", lag_value, "and", exp_def, "exposure")
  }
  
  # Create plot
  p <- ggplot(results_clean, aes(y = Model, x = HR)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbarh(
      aes(xmin = CI_lower, xmax = CI_upper),
      height = 0.2,
      position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(~ Variable, ncol = 1, scales = "free_y") +
    labs(
      title = plot_title,
      y = "Model",
      x = "Hazard Ratio (HR)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(angle = 0)
    ) +
    scale_x_log10(
      breaks = c(0.5, 0.9, 1, 1.25, 1.5, 2),
      limits = c(0.9, 1.25)
    )
  
  return(p)
}

# Full Cohort Table 
full_cohort_table <- function(data, lag_value, exp_def, table_title = NULL) {
  
  # Filter data
  filtered_data <- data %>%
    filter(
      Outcome == lag_value,
      exp_definition == exp_def
    )%>%
    mutate(
      Model = factor(Model, levels = c("A", "B", "C"))
    )
  
  # Reshape to wide format
  point_results_wide <- filtered_data %>% 
    select(Model, Variable, Exposure, 
           `HR (95% CI)`, `p-value`) %>% 
    pivot_wider(
      id_cols = c(Variable, Exposure),
      names_from = Model,
      values_from = c(`HR (95% CI)`, `p-value`),
      names_glue = "{Model}__{.value}",
      names_vary = "slowest"
    ) %>%
    arrange(Variable, Exposure)
  
  # Default title if none supplied
  if (is.null(table_title)) {
    table_title <- paste(
      "Primary Analysis:",
      exp_def,
      "with",
      lag_value
    )
  }
  
  # Create gt table
  point_table <-  point_results_wide %>%
    group_by(Variable) %>%
    gt() %>%
    
    # Automatically create column spanners based on delimiter
    tab_spanner_delim(delim = "__") %>%
    
    cols_align(align = "center", columns = everything()) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_header(title = table_title)
  
  return(point_table)
}


###################################################################

# Menopausal status 
menopause_plot <- function(data,
                           pollutant_choice,
                           lag_choice,
                           exp_def_choice) {
  
  # Automatically assign units
  units_choice <- ifelse(pollutant_choice == "NO2", "per10", "per5")
  
  # Filter dataset
  plot_data <- data %>%
    filter(
      pollutant == pollutant_choice,
      lag == lag_choice,
      exposure == exp_def_choice,
      units == units_choice
    ) %>%
    pivot_longer(
      cols = c(HR_pre, CI_lower_pre, CI_upper_pre, pvalue_pre,
               HR_post, CI_lower_post, CI_upper_post, pvalue_post),
      names_to = c(".value", "menopause"),
      names_pattern = "(HR|CI_lower|CI_upper|pvalue)_(pre|post)"
    ) %>%
    mutate(
      menopause = case_when(
        menopause == "pre" ~ "Premenopausal",
        menopause == "post" ~ "Postmenopausal"
      ),
      menopause = factor(menopause,
                         levels = c("Premenopausal", "Postmenopausal")),
      Model = factor(Model, levels = c("A", "B", "C")),
      group_label = paste(Model, pollutant, sep = " - "),
      group_label = factor(group_label,
                           levels = rev(paste(c("A","B","C"),
                                              pollutant_choice,
                                              sep = " - ")))
    )
  
  # Plot
  p <- ggplot(plot_data, aes(x = HR, y = group_label, color = menopause)) +
    geom_point(position = position_dodge(width = 0.7), size = 4) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper),
                  position = position_dodge(width = 0.7),
                  width = 0.3,
                  linewidth = 1.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_text(
      data = plot_data %>% distinct(group_label, wald_pval),
      aes(
        x = 1.25,
        y = group_label,
        label = paste0("Wald p=", sprintf("%.3f", wald_pval))
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4
    ) +
    scale_x_log10(
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
      limits = c(0.75, 2)
    ) +
    scale_color_manual(
      values = c(
        "Premenopausal" = "#fc6dab",
        "Postmenopausal" = "#83ad6c"
      )
    ) +
    theme_minimal() +
    labs(
      x = "Hazard Ratio (HR)",
      y = "Model - Pollutant",
      color = "Menopausal Status",
      title = paste(exp_def_choice, pollutant_choice,
                    "by menopausal status (", units_choice, ",", lag_choice, ")")
    ) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size = 18,
                                face = "bold",
                                hjust = 0)
    )
  
  return(p)
}


menopause_table <- function(data, lag_choice, exp_choice, pollutant_choice = NULL) {
  
  # Filter dataset
  tbl_data <- data %>%
    filter(
      lag == lag_choice,
      exposure == exp_choice,
      if(!is.null(pollutant_choice)) pollutant == pollutant_choice else TRUE
    ) %>%
    mutate(
      # Format HR (95% CI) strings
      `HR (95% CI) Pre` = paste0(round(HR_pre, 2), 
                                 " (", round(CI_lower_pre, 2), 
                                 ", ", round(CI_upper_pre, 2), ")"),
      `HR (95% CI) Post` = paste0(round(HR_post, 2), 
                                  " (", round(CI_lower_post, 2), 
                                  ", ", round(CI_upper_post, 2), ")"),
      `Wald p-value` = round(wald_pval, 3)
    ) %>%
    select(
      Model,
      Pollutant = pollutant,
      Lag = lag,
      Exposure = exposure,
      Units = units,
      `HR (95% CI) Pre`,
      pvalue_pre,
      `HR (95% CI) Post`,
      pvalue_post,
      `Wald p-value`
    ) %>%
    arrange(Pollutant, Model)
  
  # Create gt table
  gt_table <- tbl_data %>%
    gt(groupname_col = "Model") %>%
    tab_header(
      title = md("**Menopause Analysis**"),
      subtitle = paste0("HR with 95%CI and interaction p-values (Wald) - Lag: ", lag_choice,
                        ", Exposure: ", exp_choice)
    ) %>%
    cols_label(
      Model = "Model",
      Pollutant = "Pollutant",
      Lag = "Lag time",
      Exposure = "Exposure Type",
      Units = "Units",
      `HR (95% CI) Pre` = "Premenopausal HR (95% CI)",
      pvalue_pre = "Premen. p-value",
      `HR (95% CI) Post` = "Postmenopausal HR (95% CI)",
      pvalue_post = "Postmen. p-value",
      `Wald p-value` = "Wald p-value"
    ) %>%
    fmt_missing(
      columns = everything(),
      missing_text = "—"
    ) %>%
    tab_options(
      table.font.size = "small",
      column_labels.font.weight = "bold"
    )
  
  return(gt_table)
}



################################################################################################

# ER status

erstatus_plot <- function(data, pollutant_choice, lag_choice, exp_def_choice) {
  
  # Clean and parse columns
  df_clean <- data %>%
    mutate(
      Models = case_when(
        grepl("A_", Model) ~ "A", 
        grepl("B_", Model) ~ "B",
        grepl("C_", Model) ~ "C",
        TRUE ~ "ERROR"
      ), 
      Outcome = case_when(
        grepl("nolag", Model) ~ "no lag", 
        grepl("2lag", Model) ~ "2yr lag",
        grepl("5lag", Model) ~ "5yr lag",
        TRUE ~ "ERROR"
      ), 
      Exposure = case_when(
        grepl("NO2", Model) ~ "NO2", 
        grepl("P25", Model) ~ "PM2.5",
        grepl("P10", Model) ~ "PM10",
        TRUE ~ "ERROR"
      ), 
      ExpUnits = case_when(
        grepl("_10_", Model) ~ "per 10", 
        grepl("_5_", Model) ~ "per 5",
        TRUE ~ "ERROR"
      ), 
      ExpLength = case_when(
        grepl("avg", Model) ~ "average", 
        grepl("point", Model) ~ "point",
        TRUE ~ "ERROR"
      ), 
      Event = case_when(
        Subtype == "pos" ~ "Positive", 
        Subtype == "neg" ~ "Negative",
        Subtype == "unk" ~ "Unknown",
        TRUE ~ "ERROR"
      ), 
      Wald = Wald_pval,
      Model = Models
    ) %>%
    select(-Models, -Subtype)
  
  # Format HR, CI, and p-value string
  df_clean <- df_clean %>%
    mutate(
      HR_CI_p = paste0(
        sprintf("%.2f", HR),
        " (", sprintf("%.2f", CI_lower), "–", sprintf("%.2f", CI_upper), "), p=",
        formatC(Pval, format = "f", digits = 3)
      )
    )
  
  # Filter for plotting
  plot_data <- df_clean %>%
    filter(
      Exposure == pollutant_choice,
      Outcome == lag_choice,
      ExpLength == exp_def_choice,
      Event != "Unknown"
    )
  
  # Order models A->B->C
  plot_data$Model <- factor(plot_data$Model, levels = rev(c("A", "B", "C")))
  
  # Create forest plot
  p <- ggplot(plot_data, aes(
    y = Model,
    x = HR,
    xmin = CI_lower,
    xmax = CI_upper,
    color = Event
  )) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                   height = 0.2,
                   position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(~ ExpLength, ncol = 1, scales = "free_y", drop = FALSE) +
    labs(
      title = paste("Average Exposure by ER Status:", pollutant_choice, "-", lag_choice),
      y = "Model",
      x = "Hazard Ratio (HR)",
      color = "Event"
    ) +
    scale_x_log10(
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
      limits = c(0.75, 1.75)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(angle = 0)
    ) +
    geom_text(
      data = plot_data %>% distinct(Model, Wald),
      aes(x = 1.5, y = Model, label = paste0("LRT p=", signif(Wald, 3))),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3
    )
  
  return(p)
}

# The table
erstatus_table <- function(data, lag_choice, exp_def_choice) {
  
  # Clean and parse columns
  data_clean <- data %>%
    mutate(
      Models = case_when(
        grepl("A_", Model) ~ "A", 
        grepl("B_", Model) ~ "B",
        grepl("C_", Model) ~ "C",
        TRUE ~ "ERROR"
      ), 
      Outcome = case_when(
        grepl("nolag", Model) ~ "no lag", 
        grepl("2lag", Model) ~ "2yr lag",
        grepl("5lag", Model) ~ "5yr lag",
        TRUE ~ "ERROR"
      ), 
      Exposure = case_when(
        grepl("NO2", Model) ~ "NO2", 
        grepl("P25", Model) ~ "PM2.5",
        grepl("P10", Model) ~ "PM10",
        TRUE ~ "ERROR"
      ), 
      ExpUnits = case_when(
        grepl("_10_", Model) ~ "per 10", 
        grepl("_5_", Model) ~ "per 5",
        TRUE ~ "ERROR"
      ), 
      ExpLength = case_when(
        grepl("avg", Model) ~ "average", 
        grepl("point", Model) ~ "point",
        TRUE ~ "ERROR"
      ), 
      Event = case_when(
        Subtype == "pos" ~ "Positive", 
        Subtype == "neg" ~ "Negative",
        Subtype == "unk" ~ "Unknown",
        TRUE ~ "ERROR"
      ), 
      Wald = Wald_pval,
      Model = Models
    ) %>%
    select(-Models, -Subtype)
  
  # Format HR (95% CI) + p-value
  results_formatted <- data_clean %>%
    mutate(
      HR_CI_p = paste0(
        sprintf("%.2f", HR),
        " (", sprintf("%.2f", CI_lower), "–", sprintf("%.2f", CI_upper), "), p=",
        formatC(Pval, format = "f", digits = 3)
      )
    ) %>%
    select(Model, Outcome, Exposure, ExpUnits, ExpLength, Event, HR_CI_p, Wald)
  
  # Pivot Events wide
  results_wide <- results_formatted %>%
    pivot_wider(
      names_from = Event,
      values_from = HR_CI_p
    ) %>%
    group_by(Outcome, Exposure, Model, ExpUnits, ExpLength) %>%
    mutate(Wald = round(first(Wald), 4)) %>%
    ungroup() %>%
    select(ExpLength, Model, Outcome, Exposure, ExpUnits, Positive, Negative, Unknown, Wald)
  
  # Filter by lag & exposure definition
  results_filtered <- results_wide %>%
    filter(Outcome == lag_choice, ExpLength == exp_def_choice) %>%
    select(-Outcome)
  
  # Create gt table grouped by Exposure
  tbl <- results_filtered %>%
    group_by(Exposure, ExpLength) %>%
    gt(groupname_col = "Model") %>%
    tab_header(
      title = md(paste("Menopause HRs by ER status:", lag_choice)),
      subtitle = paste("Exposure type:", exp_def_choice)
    ) %>%
    tab_options(
      row_group.as_column = TRUE,
      table.font.size = 12
    ) %>%
    cols_label(
      ExpUnits = "Exposure Units",
      Positive = "Positive",
      Negative = "Negative",
      Unknown = "Unknown",
      Wald = "Wald Test (pos vs. neg)"
    )
  
  return(tbl)
}
