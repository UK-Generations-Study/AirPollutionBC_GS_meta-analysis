# Cox extraction fucntions
extract_cox <- function(model, model_name, var_name) {
  s <- summary(model)
  
  # Find the variable row (match on variable name)
  idx <- grep(var_name, rownames(s$coefficients))
  if (length(idx) == 0) stop("Variable not found in model coefficients.")
  
  # Extract HR, CI, and p-value
  hr <- s$coefficients[idx, "exp(coef)"]
  lower <- s$conf.int[idx, "lower .95"]
  upper <- s$conf.int[idx, "upper .95"]
  pval <- s$coefficients[idx, "Pr(>|z|)"]
  
  # Extract model suffix (e.g. "C" from "P10_C")
  model_suffix <- sub(".*_", "", model_name)
  
  # Return tidy data frame
  data.frame(
    Variable = var_name,
    Model = model_suffix,
    HR = hr,
    Lower_CI = lower,
    Upper_CI = upper,
    P_value = pval,
    row.names = NULL
  )
}

# Function to extract Wald test p-value for a specific interaction term
wald_pvalue <- function(cox_model, interaction_term, model_name) {
  
  # Parse model name into components
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'C_P10_int')")
  }
  model_id <- name_parts[1]
  pollutant <- name_parts[2]
  term_type <- name_parts[3]
  
  # Get the summary of the model
  s <- summary(cox_model)
  
  # Extract the coefficients table
  coef_table <- s$coefficients
  
  # Check if interaction term exists
  if (!interaction_term %in% rownames(coef_table)) {
    stop("Interaction term not found in model coefficients")
  }
  
  # Extract p-value
  p_value <- coef_table[interaction_term, "Pr(>|z|)"]
  
  # Return as single-row data frame
  data.frame(
    Model = model_id,
    pollutant = pollutant,
    term = term_type,
    wald_pval = p_value
  )
}

wald_pvalue_carstairs <- function(cox_model, model_name, exclude_unknown = TRUE) {
  
  name_parts <- unlist(strsplit(model_name, "_"))
  if (length(name_parts) != 3) {
    stop("Model name must have three parts separated by underscores (e.g., 'C_P25_int')")
  }
  
  model_id  <- name_parts[1]
  pollutant <- name_parts[2]
  term_type <- name_parts[3]
  
  pollutant_var <- paste0("avg", pollutant)
  
  coefs    <- coef(cox_model)
  vcov_mat <- vcov(cox_model)
  
  # --- Dynamically detect all Carstairs interaction terms ---
  int_pattern    <- paste0("^", pollutant_var, ":Carstairs_quint")
  int_term_names <- names(coefs)[grepl(int_pattern, names(coefs))]
  
  if (length(int_term_names) == 0) {
    stop(paste("No Carstairs interaction terms found in model:", model_name))
  }
  
  
  # --- Optionally exclude Unknown ---
  if (exclude_unknown) {
    int_term_names <- int_term_names[!grepl("Unknown", int_term_names)]
  } else {

  }
  # --- Global Wald test across remaining interaction terms ---
  beta_wald <- coefs[int_term_names]
  vcov_wald <- vcov_mat[int_term_names, int_term_names, drop = FALSE]
  
  wald_stat <- as.numeric(t(beta_wald) %*% solve(vcov_wald) %*% beta_wald)
  wald_pval <- pchisq(wald_stat, df = length(int_term_names), lower.tail = FALSE)
  
  data.frame(
    Model         = model_id,
    pollutant     = pollutant,
    term          = term_type,
    wald_stat     = round(wald_stat, 4),
    df            = length(int_term_names),
    wald_pval     = round(wald_pval, 4),
    exclude_unknown = exclude_unknown
  )
}
