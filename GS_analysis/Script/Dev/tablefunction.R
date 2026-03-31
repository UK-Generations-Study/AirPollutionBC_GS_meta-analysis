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
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(
      height = 0.2,
      position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(~ pollutant, ncol = 1, scales = "free_y", drop = FALSE) +
    labs(
      title = "",
      y = "Model",
      x = "Hazard Ratio (HR)",
      color = "Event"
    ) +
    scale_x_log10(
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
      limits = c(0.75, 1.75)
    ) +
    scale_color_manual(
      breaks = if(type == "Menopausal Status") {c("Premenopausal", "Postmenopausal")} else {c("ER Positive", "ER Negative")},
      values = if(type == "Menopausal Status") {c("Premenopausal" = "#1f78b4","Postmenopausal" = "#e31a1c")} else {c("ER Positive" = "#1f78b4", "ER Negative"= "#e31a1c")}
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(angle = 0)
    ) +
    geom_text(
      data = plot_data %>% distinct(Model, wald_pval),
      aes(x = 1.5, y = Model, label = paste0("Wald p=", signif(wald_pval, 3))),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4
    )

  return(p)
}

x <- subanalysis_plot(lunnmcneil_results, type = "ER Status")
x
y <- subanalysis_plot(all_menopause, type = "Menopausal Status")
y
