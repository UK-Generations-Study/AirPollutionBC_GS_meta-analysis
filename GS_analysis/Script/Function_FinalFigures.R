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
    plot_title <- paste("")
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

###################################################################
# Subtype Plot

subtype_plot <- function(df,
         x_breaks = c( 0.5, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.5, 3, 3.5),
         show_wald = TRUE,
         plot_width = 12,
         plot_height = NULL) {
  
  # ── prep ────────────────────────────────────────────────────────────────────
  
  if(show_wald == TRUE){
    model_levels <- c("Final Model", "Menopausal Status", "ER Status", "Invasive vs. DCIS", "Screen Detected", "Carstairs Index") 
  } else {
    model_levels <- c("Point Exposure", "2 year time lag", "5 year time lag", "EXPANSE Model Accuracy","Single Covariate Analysis" ) 
  }
  df <- df %>%
    mutate(
      stat_label = paste0(
        formatC(HR,       format = "f", digits = 2), " (",
        formatC(CI_lower, format = "f", digits = 2), "\u2013",
        formatC(CI_upper, format = "f", digits = 2), ")  p=",
        formatC(Pval,     format = "f", digits = 3)
      ),
      Model = factor(Model, levels = model_levels),
      row_key = paste(Model, Subtype, sep = "__") 
    )
  
  # ── y-axis order: preserve input order, top-to-bottom ───────────────────────
  row_order <- df %>%
    mutate(orig_order = row_number()) %>%
    arrange(desc(orig_order)) %>%
    pull(row_key)
  
  df <- df %>%
    mutate(row_key = factor(row_key, levels = row_order))
  
  # ── axis limits ─────────────────────────────────────────────────────────────
  x_limits <- c(
    min(df$CI_lower, na.rm = TRUE) * 0.85,
    max(df$CI_upper, na.rm = TRUE) * 1.15
  )
  x_text_max <- max(x_limits) * 2.2
  
  # ── colours: one per model ──────────────────────────────────────────────────
  palette <- c("#185FA5", "#0F6E56", "#993C1D", "#7F77DD", "#BA7517",
               "#D4537E", "#BA7517", "#3B6D11")
  models  <- unique(df$Model)
  model_colors <- setNames(palette[seq_along(models)], models)
  
  # ── plot ────────────────────────────────────────────────────────────────────
  p <- ggplot(df, aes(x = HR, y = row_key, colour = Model)) +
    
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    
    geom_errorbarh(
      aes(xmin = CI_lower, xmax = CI_upper),
      height = 0.25, linewidth = 0.7
    ) +
    
    geom_point(shape = 15, size = 3) +
    
    geom_text(
      aes(x = max(x_limits) * 1.02, label = stat_label),
      hjust = 0, size = 5, colour = "grey30"
    ) +
    
    scale_x_log10(
      breaks = x_breaks,
      labels = x_breaks,
      limits = c(x_limits[1], x_text_max)
    ) +
    scale_colour_manual(values = model_colors) +
    
    scale_y_discrete(labels = function(x) sub(".*__", "", x)) +
    
    facet_grid(
      rows   = vars(Model),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    
    labs(x = "Hazard ratio (log scale)", y = NULL, colour = NULL) +
    
    theme_classic(base_size = 11) +
    theme(
      strip.placement   = "outside",
      strip.background  = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1,
                                       face = "bold", size = 15),
      panel.spacing     = unit(0.8, "lines"),
      axis.text.y       = element_text(size = 12),
      axis.line.y       = element_blank(),
      axis.ticks.y      = element_blank(),
      legend.position   = "none",
      plot.margin       = margin(10, 10, 10, 10)
    )
  
  # ── optional wald label ──────────────────────────────────────────────────────
  if (show_wald) {
    wald_labels <- df %>%
      group_by(Model) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      mutate(
        wald_text = paste0("Wald p-value = ",
                           formatC(Wald_pval, format = "f", digits = 3))
      )
    
    p <- p + geom_text(
      data = wald_labels,
      aes(x = 1, y = row_key, label = wald_text),
      hjust = 0.5, vjust = 2.5, size = 5,
      colour = "grey10", fontface = "italic",
      inherit.aes = FALSE
    )
  }
  
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


