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
full_cohort_table <- function(data, table_title = NULL) {
  
  # Filter data
  filtered_data <- data %>%
    mutate(
      included = case_when(
        exp_definition == "average" & Outcome == "nolag" ~ 1, 
        exp_definition == "average" & Outcome == "2lag" ~ 2, 
        exp_definition == "average" & Outcome == "5lag" ~ 3, 
        exp_definition == "point" & Outcome == "nolag" ~ 4, 
        TRUE ~ 0
      ), 
      Analysis = case_when(
        included == 1 ~ "Average Exposure", 
        included == 2 ~ "2 year lag", 
        included == 3 ~ "5 year lag", 
        included == 4 ~ "Point Exposure", 
        TRUE ~ "out"
      ),
      Model = factor(Model, levels = c("A", "B", "C")), 
      Analysis = factor(Analysis, levels = c("Average Exposure", "Point Exposure", "2 year lag", "5 year lag"))
    ) %>%
    filter(
      included != 0
    ) %>%
    select (
      -included
    )
  
  # Reshape to wide format
  point_results_wide <- filtered_data %>% 
    select(Model, Variable, Analysis, Exposure, 
           `HR (95% CI)`, `p-value`) %>% 
    pivot_wider(
      id_cols = c(Model, Variable, Exposure),
      names_from = Analysis,
      values_from = c(`HR (95% CI)`, `p-value`),
      names_glue = "{Analysis}__{.value}"
    ) %>%
    arrange(Model, Variable, Exposure) 
  
  point_results_wide <- point_results_wide %>%
    select(
      Model, Variable, Exposure,
      
      `Average Exposure__HR (95% CI)`,
      `Average Exposure__p-value`,
      
      `Point Exposure__HR (95% CI)`,
      `Point Exposure__p-value`,
      
      `2 year lag__HR (95% CI)`,
      `2 year lag__p-value`,
      
      `5 year lag__HR (95% CI)`,
      `5 year lag__p-value`
    )
  
  # Create gt table
  table <- point_results_wide %>%
    gt(rowname_col = "Variable", groupname_col = "Model") %>%
    
    # Create column groups by Analysis
    tab_spanner_delim(delim = "__") %>%
    
    cols_align(align = "center", columns = everything()) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    
    tab_header(title = table_title)
  
  return(table)
}


###################################################################
# Subtype Plot

subanalysis_plot <- function(data, type = "Menopausal Status", lag_choice = "no years",exp_choice = "average") {
  if(type == "ER Status"){
    data_clean <- data %>%
      mutate(
        pval = Pval, 
        wald_pval = Wald_pval
      )%>%
      select(
        -Pval, - Wald_pval
      ) %>%
      pivot_wider(
        names_from = Subtype,
        values_from = c(HR, CI_lower, CI_upper, pval),
        names_sep = "_"
      ) %>%
      separate(
        Model,
        into = c("Model", "pollutantplus", "units", "extra", "lag"),
        sep = "_"
      )%>%
      mutate(
        pollutant = case_when(
          grepl("P25", pollutantplus) ~ "PM 2.5",
          grepl("P10", pollutantplus) ~ "PM 10",
          TRUE ~ "ERROR"
        ), 
        exposure = case_when(
          grepl("avg", pollutantplus) ~ "average",
          grepl("point", pollutantplus) ~ "point",
          TRUE ~ "ERROR"
        ), 
        units = case_when(
          grepl("10", units) ~ "per10",
          grepl("5", units) ~ "per5",
          TRUE ~ "ERROR"
        ), 
        lag = case_when(
          grepl("no", lag) ~ "no years",
          grepl("2", lag) ~ "2 years",
          grepl("5", lag) ~ "5 years",
          TRUE ~ "ERROR"
        )
      ) %>%
      select(
        -pollutantplus, -extra
      ) %>%
      filter(
        lag == lag_choice,
        exposure == exp_choice) 
    
    
    plot_data <- data_clean %>%
      pivot_longer(
        cols = c(HR_pos, HR_neg, HR_unk,
                 CI_lower_pos, CI_lower_neg, CI_lower_unk,
                 CI_upper_pos, CI_upper_neg, CI_upper_unk,
                 pval_pos, pval_neg, pval_unk),
        names_to = c(".value", "Event"),
        names_pattern = "(HR|CI_lower|CI_upper|pval)_(pos|neg|unk)"
      )%>% 
      filter(
        Event != "unk"
      ) %>%
      mutate(
        Event = case_when(
          Event == "pos" ~ "ER Positive", 
          Event == "neg" ~ "ER Negative", 
          TRUE ~ "Error"
        ),
        Event = factor(Event, levels = rev(c("ER Positive", "ER Negative"))), 
        Model = factor(Model, levels = rev(c("A", "B", "C")))
      )
    
  } else if(type == "Menopausal Status"){
    
    data_clean <- data %>%
      mutate(
        pollutant = case_when(
          grepl("P25", pollutant) ~ "PM 2.5",
          grepl("P10", pollutant) ~ "PM 10",
          TRUE ~ "ERROR"
        )) %>%
      filter(
        lag == lag_choice,
        exposure == exp_choice
      ) 
    
    plot_data <- data_clean %>%
      pivot_longer(
        cols = c(HR_pre, HR_post, 
                 CI_lower_pre, CI_lower_post,
                 CI_upper_pre, CI_upper_post,
                 pvalue_pre, pvalue_post),
        names_to = c(".value", "Event"),
        names_pattern = "(HR|CI_lower|CI_upper|pvalue)_(pre|post)"
      )%>% 
      mutate(
        Event = case_when(
          Event == "pre" ~ "Premenopausal", 
          Event == "post" ~ "Postmenopausal", 
          TRUE ~ "Error"
        ),
        Event = factor(Event, levels = rev(c("Premenopausal", "Postmenopausal"))), 
        Model = factor(Model, levels = rev(c("A", "B", "C")))
      )
    
  } else {
    stop( "ERROR: options are only ER Status or Menopausal Status")
  }
  
  p <- ggplot(plot_data, aes(
    y = Model,
    x = HR,
    xmin = CI_lower,
    xmax = CI_upper,
    color = Event, 
    group = Event
  )) +
    geom_point(size = 4, position = position_dodge(width = 0.5)) +
    geom_errorbarh(
      height = 0.1,
      linewidth = 0.7,
      position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(~ pollutant, ncol = 2, scales = "free_y", drop = FALSE) +
    labs(
      title = "",
      y = "Model",
      x = "Hazard Ratio (HR)",
      color = "Event"
    ) +
    scale_x_log10(
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
      limits = c(0.75, 2)
    ) +
    scale_color_manual(
      breaks = if(type == "Menopausal Status") {c("Premenopausal", "Postmenopausal")} else {c("ER Positive", "ER Negative")},
      values = if(type == "Menopausal Status") {c("Premenopausal" = "#2a9d8f","Postmenopausal" = "#e76f51")} else {c("ER Positive" = "#2a9d8f", "ER Negative"= "#e76f51")}
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(angle = 0)
    ) +
    geom_text(
      data = plot_data %>% distinct(pollutant, Model, wald_pval),
      aes(x = 1.5, y = Model, label = paste0("Wald p=", signif(wald_pval, 3))),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4
    )
  
  return(p)
}

#################################
# Subtype Table

subanalysis_table <- function(data, type = "Menopausal Status",lag_choice = "no years",exp_choice = "average") {
  if(type == "ER Status"){
    data_clean <- data %>%
      mutate(
        pval = Pval, 
        wald_pval = Wald_pval
      )%>%
      select(
        -Pval, - Wald_pval
      ) %>%
      pivot_wider(
        names_from = Subtype,
        values_from = c(HR, CI_lower, CI_upper, pval),
        names_sep = "_"
      ) %>%
      separate(
        Model,
        into = c("Model", "pollutantplus", "units", "extra", "lag"),
        sep = "_"
      )%>%
      mutate(
        pollutant = case_when(
          grepl("P25", pollutantplus) ~ "PM 2.5",
          grepl("P10", pollutantplus) ~ "PM 10",
          TRUE ~ "ERROR"
        ), 
        exposure = case_when(
          grepl("avg", pollutantplus) ~ "average",
          grepl("point", pollutantplus) ~ "point",
          TRUE ~ "ERROR"
        ), 
        units = case_when(
          grepl("10", units) ~ "per10",
          grepl("5", units) ~ "per5",
          TRUE ~ "ERROR"
        ), 
        lag = case_when(
          grepl("no", lag) ~ "no years",
          grepl("2", lag) ~ "2 years",
          grepl("5", lag) ~ "5 years",
          TRUE ~ "ERROR"
        )
      ) %>%
      select(
        -pollutantplus, -extra
      ) %>%
      filter(
        lag == lag_choice,
        exposure == exp_choice
      ) %>%
      mutate(
        # Format HR (95% CI) strings
        `HR (95% CI) Positive` = paste0(round(HR_pos, 2), 
                                        " (", round(CI_lower_pos, 2), 
                                        ", ", round(CI_upper_pos, 2), ")"),
        `HR (95% CI) Negative` = paste0(round(HR_neg, 2), 
                                        " (", round(CI_lower_neg, 2), 
                                        ", ", round(CI_upper_neg, 2), ")"),
        `HR (95% CI) Unknown` = paste0(round(HR_unk, 2), 
                                       " (", round(CI_lower_unk, 2), 
                                       ", ", round(CI_upper_unk, 2), ")"),
        `Wald p-value` = round(wald_pval, 3)
      ) %>%
      select(
        Model,
        Pollutant = pollutant,
        Lag = lag,
        Exposure = exposure,
        Units = units,
        `HR (95% CI) Positive`,
        pval_pos,
        `HR (95% CI) Negative`,
        pval_neg,
        `HR (95% CI) Unknown`,
        pval_unk,
        `Wald p-value`
      ) %>%
      arrange(Pollutant, Model) 
    
    gt_table <- data_clean %>%
      select(-Lag, -Exposure, -Units) %>%
      gt(groupname_col = "Model") %>%
      tab_header(
        title = paste0("Subtype analysis by ", type),
        subtitle = paste0(
          "HR with 95%CI and interaction p-values (Wald) per 5 ug/m3 - Lag: ",
          lag_choice, ", Exposure: ", exp_choice
        )
      ) %>%
      
      tab_spanner(
        label = "ER Positive",
        columns = c(`HR (95% CI) Positive`, pval_pos)
      ) %>%
      tab_spanner(
        label = "ER Negative",
        columns = c(`HR (95% CI) Negative`, pval_neg)
      ) %>%
      tab_spanner(
        label = "ER Unknown",
        columns = c(`HR (95% CI) Unknown`, pval_unk)
      ) %>%
      cols_label(
        Pollutant = "Pollutant",
        `HR (95% CI) Positive` = "HR (95% CI)",
        pval_pos = "p-value",
        `HR (95% CI) Negative` = "HR (95% CI)",
        pval_neg = "p-value",
        `HR (95% CI) Unknown` = "HR (95% CI)",
        pval_unk = "p-value",
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
  } else if(type == "Menopausal Status"){
    
    data_clean <- data %>%
      filter(
        lag == lag_choice,
        exposure == exp_choice,
      ) %>%
      mutate(
        pollutant = case_when(
          grepl("P25", pollutant) ~ "PM 2.5",
          grepl("P10", pollutant) ~ "PM 10",
          TRUE ~ "ERROR"
        ),
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
    
    gt_table <- data_clean %>%
      select(-Lag, -Exposure, -Units) %>%
      gt(groupname_col = "Model") %>%
      tab_header(
        title = paste0("Subtype analysis by ", type),
        subtitle = paste0(
          "HR with 95%CI and interaction p-values (Wald) per 5 ug/m3 - Lag: ",
          lag_choice, ", Exposure: ", exp_choice
        )
      ) %>%
      
      tab_spanner(
        label = "Premenopausal",
        columns = c(`HR (95% CI) Pre`, pvalue_pre)
      ) %>%
      tab_spanner(
        label = "Postmenopausal",
        columns = c(`HR (95% CI) Post`, pvalue_post)
      ) %>%
      cols_label(
        Pollutant = "Pollutant",
        `HR (95% CI) Pre` = "HR (95% CI)",
        pvalue_pre = "p-value",
        `HR (95% CI) Post` = "HR (95% CI)",
        pvalue_post = "p-value",
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
  } else {
    stop( "ERROR: options are only ER Status or Menopausal Status")
  }
  
  
  return(gt_table)
  
}


