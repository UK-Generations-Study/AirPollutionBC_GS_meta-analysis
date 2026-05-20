presentation_plot <- function(df,
                              x_breaks = c(0.5, 0.8, 0.9, 1.0, 1.2, 1.4),
                              show_wald = TRUE,
                              model_levels = NULL,
                              plot_width = 12,
                              plot_height = NULL) {
  
  # ── prep ────────────────────────────────────────────────────────────────────
  
  if (is.null(model_levels)) {
    if (show_wald == TRUE) {
      model_levels <- c("Final Model", "Menopausal Status", "Oestrogen Receptor (ER) Status", "Invasive vs. DCIS", "Mode of Detection*", "Carstairs Index")
    } else {
      model_levels <- c("Point Exposure", "2 year time lag", "5 year time lag", "EXPANSE Model Accuracy", "Addresses at the Household Level Only", "Single Covariate Analysis")
    }
  }
  
  df <- df %>%
    filter(Model %in% model_levels) %>%
    mutate(
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
  x_text_max <- max(x_limits) * 3.2
  
  # ── colours: one per model ──────────────────────────────────────────────────
  palette <- c("#185FA5", "#0F6E56", "#993C1D", "#7F77DD", "#BA7517",
               "#D4537E", "#BA7517", "#3B6D11")
  models  <- unique(df$Model)
  model_colors <- setNames(palette[seq_along(models)], models)
  
  # ── wald labels (one per facet, shown at right) ──────────────────────────────
  if(show_wald){
    wald_labels <- df %>%
    group_by(Model) %>%
    mutate(n = n(), mid_idx = ceiling(n / 2)) %>%
    slice(ceiling(n() / 2)) %>%
    ungroup() %>%
    mutate(
      wald_text = paste0("Test for Heterogeneity = ", formatC(Wald_pval, format = "f", digits = 3))
    )
  }
  
  # ── plot ────────────────────────────────────────────────────────────────────
  p <- ggplot(df, aes(x = HR, y = row_key, colour = Model)) +
    
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.8) +
    
    geom_errorbarh(
      aes(xmin = CI_lower, xmax = CI_upper),
      height = 0.25, linewidth = 1.4
    ) +
    
    geom_point(shape = 15, size = 5) +
    
    # Wald p-value on the right instead of HR(CI)
   {if (show_wald) geom_text(
      data = wald_labels,
      aes(x = max(x_limits) * 1.02, y = row_key, label = wald_text),
      hjust = 0, size = 7, colour = "grey20",
      fontface = "italic",
      inherit.aes = FALSE
    ) 
     } +
    
    scale_x_log10(
      breaks = x_breaks,
      labels = x_breaks,
      #limits = c(x_limits[1], x_text_max), 
      limits = c(0.9, 1.40)
    ) +
    scale_colour_manual(values = model_colors) +
    
    scale_y_discrete(labels = function(x) sub(".*__", "", x)) +
    
    facet_grid(
      rows   = vars(Model),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    
    labs(x = "Hazard ratio (per 5 ug/m3)", y = NULL, colour = NULL) +
    
    theme_classic(base_size = 14) +
    theme(
      strip.placement   = "outside",
      strip.background  = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1,
                                       face = "bold", size = 25),
      panel.spacing     = unit(1.2, "lines"),
      axis.text.y       = element_text(size = 18),
      axis.line.y       = element_blank(),
      axis.ticks.y      = element_blank(),
      axis.text.x       = element_text(size = 16),
      axis.title.x      = element_text(size = 20),
      legend.position   = "none",
      plot.margin       = margin(10, 10, 10, 10)
    )
  
  return(p)
}

subype_clean <- subtype %>%
  mutate(
    Subtype = case_when (
      Model == "Final Model" ~ Exposure, 
      TRUE ~ Subtype
    )
  )

All <- subtype_P25 %>%
  select(-Wald_pval)
All<-  rbind(All, sensitivity_P25_clean)

finalmodel_plot <- presentation_plot(subype_clean,
                                      model_levels = c("Final Model"),
                                      x_breaks = c(0.1, 0.3, 0.5, 0.8, 0.9, 1.0, 1.05, 1.10, 1.15, 1.2, 1.25))
finalmodel_plot


subgroup_P25_plot <- presentation_plot(subtype_P25,
                                      model_levels = c("Menopausal Status", "Carstairs Index"),
                                 x_breaks = c(0.1, 0.3, 0.5, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0))
subgroup_P25_plot

subtype_P25_plot <- presentation_plot(subtype_P25,
                                      model_levels = c("Oestrogen Receptor (ER) Status", "Invasive vs. DCIS", "Mode of Detection*"),
                                      x_breaks = c(0.1, 0.3, 0.5, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0))
subtype_P25_plot





sensitivity_P25_1_plot <- presentation_plot(All, 
                                          model_levels = c("Final Model", "EXPANSE Model Accuracy", "2 year time lag", "5 year time lag"),
                                          show_wald = FALSE,
                                          x_breaks = c( 0.95, 1.0, 1.05, 1.10, 1.15, 1.2, 1.25, 1.30, 1.35))
sensitivity_P25_1_plot

sensitivity_P25_2_plot <- presentation_plot(All, 
                                          model_levels = c("Single Covariate Analysis"),
                                          show_wald = FALSE,
                                          x_breaks = c( 0.95, 1.0, 1.05, 1.10, 1.15, 1.2, 1.25))
sensitivity_P25_2_plot


plot_distribution <- function(df, var, bins = 30, color = "#6443D1") {
  library(ggplot2)
  library(dplyr)
  
  x <- df[[var]]
  
  if (!is.numeric(x)) stop(paste(var, "must be a numeric (continuous) variable."))
  
  stats <- list(
    mean   = mean(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    n      = sum(!is.na(x))
  )
  
  subtitle <- sprintf(
    "n = %d | Mean = %.2f | SD = %.2f",
    stats$n, stats$mean, stats$sd
  )
  
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = bins, fill = color, alpha = 0.7, color = "white") +
    geom_density(color = "black", linewidth = 0.8) +
    geom_vline(xintercept = stats$mean,   color = "black",  linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    labs(
      title    = paste("Distribution of", var),
      subtitle = subtitle,
      x        = var,
      y        = "Density"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.subtitle = element_text(size = 10, color = "black"))
}
plot_distribution(complete_cohort, "avgP25")
plot_distribution(complete_cohort, "avgP10")

plot_correlation <- function(df, x_var = "avgP25", y_var = "avgP10") {
  library(ggplot2)
  
  x <- df[[x_var]]
  y <- df[[y_var]]
  
  # Compute correlation stats
  cor_val <- cor(x, y, use = "complete.obs")
  cor_test <- cor.test(x, y, use = "complete.obs")
  p_val <- cor_test$p.value
  
  p_label <- ifelse(p_val < 0.001, "p < 0.001", sprintf("p = %.3f", p_val))
  subtitle <- sprintf("Pearson r = %.3f | %s | n = %d", cor_val, p_label, sum(complete.cases(x, y)))
  
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.4, color = "#6443D1", size = 1.8) +
    geom_smooth(method = "lm", color = "grey20", se = TRUE, linewidth = 0.9) +
    labs(
      title    = paste("Correlation:", x_var, "vs", y_var),
      subtitle = subtitle,
      x        = x_var,
      y        = y_var
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.subtitle = element_text(size = 9, color = "grey40"))
}

# Run it
plot_correlation(complete_cohort)
