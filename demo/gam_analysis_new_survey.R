# Demo script for gam_contact_matrix with hypothetical new survey data
# Analyzing Age x Ethnicity and Age x Socioeconomic Status (SES) independently

# --- Load Libraries ---
message("Loading required libraries...")
library(socialmixr)
library(mgcv)
library(data.table)
library(ggplot2)
library(patchwork)

# --- Load and Prepare Data ---
message("Loading survey data...")
# !!! USER ACTION REQUIRED !!!
# Replace the line below with code to load your actual survey object.
# Ensure it's a 'contact_survey' object with columns like:
# part_id, part_age, cnt_age, part_ethnicity, cnt_ethnicity, part_ses, cnt_ses
# Note: Ethnicity and SES columns should ideally be factors.
# Example:
# new_survey_data <- readRDS("path/to/your/survey_data.rds")
# Or load from other formats and convert using survey()
new_survey_data <- readRDS('data/connect_survey.rds')

# Check if data is loaded
# if (is.null(new_survey_data) || !inherits(new_survey_data, "contact_survey")) {
#   stop("Failed to load survey data or data is not a contact_survey object. Please check the loading step.")
# }
message("Survey data loaded.")

# Ensure the gam_contact_matrix function is available
# Assumes the R script is in the workspace root
source("R/contact_matrix.R")

# --- General Configuration ---
# Optional: Specify country if relevant
target_country <- NULL # e.g., "United Kingdom" or keep NULL if not needed

# Optional: Specify common age limits for analyses
analysis_age_limits <- c(0, 80) # e.g., 0 to 80 years

# GAM settings (can be customized)
gam_family_setting <- nb() # Negative Binomial often suitable
k_tensor_setting <- c(8, 8)
k_by_setting <- 6
bs_numeric_setting <- "ps"

# --- Analysis 1: Age x Ethnicity ---
message("\n--- Starting Analysis 1: Age x Ethnicity ---")

# Define dimensions for Age x Ethnicity
dimensions_age_eth <- c("part_age", "cnt_age", "part_ethnicity", "cnt_ethnicity")

# !!! USER ACTION REQUIRED !!!
# Define breaks for Age x Ethnicity
# Adjust age breaks and ethnicity levels as needed for your data
dim_breaks_age_eth <- list(
  part_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  cnt_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  part_ethnicity = c('White','Asian','Black','Mixed','Other'), # Replace with actual factor levels
  cnt_ethnicity = c('White','Asian','Black','Mixed','Other')  # Replace with actual factor levels
)

# Run gam_contact_matrix for Age x Ethnicity
gam_results_age_eth <- tryCatch({
  gam_contact_matrix(
    survey = new_survey_data,
    countries = target_country, # Use NULL if country filtering not needed
    dimensions = dimensions_age_eth,
    dim_breaks = dim_breaks_age_eth,
    family = gam_family_setting,
    age_limits = analysis_age_limits,
    k_tensor = k_tensor_setting,
    k_by = k_by_setting,
    bs_numeric = bs_numeric_setting
  )
}, error = function(e) {
  message("Error running gam_contact_matrix for Age x Ethnicity: ", e$message)
  return(NULL)
})

# --- Process and Visualize Age x Ethnicity Results ---
if (!is.null(gam_results_age_eth)) {
  message("Age x Ethnicity GAM analysis complete. Processing results...")

  # Extract matrix and prediction grid
  predicted_matrix_age_eth <- gam_results_age_eth$matrix
  plot_data_age_eth <- gam_results_age_eth$prediction_grid
  plot_data_age_eth$predicted_contacts <- as.vector(predicted_matrix_age_eth)
  setDT(plot_data_age_eth)

  cat("\nDimensions of the Age x Ethnicity predicted matrix:\n")
  print(dim(predicted_matrix_age_eth))
  print(dimnames(predicted_matrix_age_eth))

  # Generate age labels from breaks for plotting
  age_labels_part <- paste0("[", dim_breaks_age_eth$part_age[-length(dim_breaks_age_eth$part_age)], ",", dim_breaks_age_eth$part_age[-1], ")")
  age_labels_cnt <- paste0("[", dim_breaks_age_eth$cnt_age[-length(dim_breaks_age_eth$cnt_age)], ",", dim_breaks_age_eth$cnt_age[-1], ")")

  # Factorise age groups for plotting
  # Note: This assumes regular 5-year bands based on seq(..., 5)
  plot_data_age_eth[, part_age_group := factor(paste0("[", findInterval(part_age, dim_breaks_age_eth$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dim_breaks_age_eth$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data_age_eth[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dim_breaks_age_eth$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dim_breaks_age_eth$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]

  # --- Visualization (Example: Facet wrap by ethnicity pair) ---
  message("\nCreating Age x Ethnicity visualisations...")
  # Ensure factor levels match dim_breaks for faceting labels
  eth_levels <- dim_breaks_age_eth$part_ethnicity
  plot_data_age_eth[, part_ethnicity := factor(part_ethnicity, levels = eth_levels)]
  plot_data_age_eth[, cnt_ethnicity := factor(cnt_ethnicity, levels = eth_levels)]

  p_age_eth <- ggplot(plot_data_age_eth, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts") +
    facet_grid(cnt_ethnicity ~ part_ethnicity, labeller = label_both) +
    labs(
      title = "Predicted Contacts: Age x Ethnicity",
      subtitle = paste(ifelse(!is.null(target_country), paste("Country:", target_country), "All Countries"),
                     "| Model: GAM (Age-Age Tensor + Eth Interactions + Age-Eth Smooths)"),
      x = "Participant Age Group",
      y = "Contact Age Group"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(size = 8, face = "bold")
    )

  # --- Save Age x Ethnicity Outputs ---
  output_dir_age_eth <- "output/gam_analysis/age_ethnicity"
  dir.create(output_dir_age_eth, recursive = TRUE, showWarnings = FALSE)
  
  plot_data_age_eth <- arrange(plot_data_age_eth, part_age, cnt_age)
  plot_data_age_eth$part_age_group <- factor(plot_data_age_eth$part_age_group, levels = rev(unique(plot_data_age_eth$part_age_group)))
  plot_data_age_eth$cnt_age_group <- factor(plot_data_age_eth$cnt_age_group, levels = unique(plot_data_age_eth$cnt_age_group))
  
  flattened_matrix <- ggplot(plot_data_age_eth, 
                             aes(x = part_ethnicity, y = cnt_ethnicity, fill = predicted_contacts)) + 
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Mean contacts", 
                         limits = c(0, max(plot_data$predicted_contacts, na.rm = TRUE)),
                         trans = scales::pseudo_log_trans(sigma = 2), breaks = c(0,1,2,3,4,5,6)) +
    labs(
      title = sprintf("Mean Contacts: Age Group x Ethnicity"),
      subtitle = paste("Model: GAM (Tensor Spline + Ethnicity Interaction)"),
      x = "Participant",
      y = "Contact"
    ) +
    theme_minimal(base_size = 10) +
    facet_grid(part_age_group ~ cnt_age_group, switch = 'both') + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=7),
      axis.text.y = element_text(size=7),
      legend.position = "right",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9),
      panel.spacing = unit(0, "lines"), 
      strip.background = element_blank(),
      strip.placement = "outside"
    )
  ggsave(paste0(output_dir_age_eth/, "flattened_matrix.png"), plot = flattened_matrix, width = 12, height = 10, dpi = 600, bg="white")

  plot_filename_base_age_eth <- file.path(output_dir_age_eth, paste0("gam_contact_matrix_age_ethnicity", ifelse(!is.null(target_country), paste0("_", target_country), "")))
  ggsave(paste0(plot_filename_base_age_eth, ".png"), plot = p_age_eth, width = 10, height = 8, dpi = 600, bg = "white")
  ggsave(paste0(plot_filename_base_age_eth, ".pdf"), plot = p_age_eth, width = 10, height = 8, dpi = 600, bg = "white")
  message(paste("Age x Ethnicity plot saved to:", paste0(plot_filename_base_age_eth, ".png/.pdf")))

  rds_filename_age_eth <- file.path(output_dir_age_eth, paste0("gam_results_age_ethnicity", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".rds"))
  saveRDS(gam_results_age_eth, file = rds_filename_age_eth)
  message(paste("Age x Ethnicity GAM results object saved to:", rds_filename_age_eth))

  if (interactive()) print(p_age_eth)

  # --- Compare Models (Age x Ethnicity) ---
  message("\nComparing GAM models for Age x Ethnicity...")
  # Re-aggregate data and fit models directly for comparison

  # 1. Prepare data (similar to internal steps of gam_contact_matrix)
  data_prep_eth <- copy(new_survey_data)
  # Apply country filter if needed
  if (!is.null(target_country) && "country" %in% names(data_prep_eth$participants)) {
    data_prep_eth$participants <- data_prep_eth$participants[country %in% target_country]
    data_prep_eth$contacts <- data_prep_eth$contacts[part_id %in% data_prep_eth$participants$part_id]
  }
  # Ensure age/ethnicity columns exist (basic check, assumes factors/integers)
  required_dims_eth <- dimensions_age_eth
  # Apply age limits
  if (!is.null(analysis_age_limits)) {
      min_age <- analysis_age_limits[1]
      max_age <- analysis_age_limits[2]
      data_prep_eth$participants <- data_prep_eth$participants[part_age >= min_age & part_age <= max_age]
      data_prep_eth$contacts <- data_prep_eth$contacts[cnt_age >= min_age & cnt_age <= max_age]
      part_ids_remain <- unique(data_prep_eth$participants$part_id)
      data_prep_eth$contacts <- data_prep_eth$contacts[part_id %in% part_ids_remain]
      contact_ids_remain <- unique(data_prep_eth$contacts$part_id)
      data_prep_eth$participants <- data_prep_eth$participants[part_id %in% contact_ids_remain]
  }
  # Merge
  gam_data_full_direct_eth <- merge(
      data_prep_eth$contacts,
      data_prep_eth$participants[, .(part_id, part_age, part_ethnicity)], # Select needed cols
      by = "part_id"
  )
  # Convert factors if needed (should be factors already ideally)
  gam_data_full_direct_eth[, part_ethnicity := as.factor(part_ethnicity)]
  gam_data_full_direct_eth[, cnt_ethnicity := as.factor(cnt_ethnicity)]
  # Aggregate
  gam_data_agg_direct_eth <- gam_data_full_direct_eth[, .N, by = required_dims_eth]
  message("Data prepared for direct model fitting (Age x Ethnicity).")

  # 2. Define Formulas
  # Complex formula (matches default gam_contact_matrix behaviour)
  complex_formula_eth_str <- paste(
    paste0("N ~ te(part_age, cnt_age, k = c(", k_tensor_setting[1], ", ", k_tensor_setting[2], "), bs = \"", bs_numeric_setting, "\")"),
    "interaction(part_ethnicity, cnt_ethnicity)",
    paste0("s(part_age, by = interaction(part_ethnicity, cnt_ethnicity), k = ", k_by_setting, ", bs = \"", bs_numeric_setting, "\")"),
    paste0("s(cnt_age, by = interaction(part_ethnicity, cnt_ethnicity), k = ", k_by_setting, ", bs = \"", bs_numeric_setting, "\")"),
    sep = " + "
  )
  complex_formula_eth <- as.formula(complex_formula_eth_str)

  # Simple formula (no age-by-ethnicity smooths)
  simple_formula_eth_str <- paste(
      paste0("N ~ te(part_age, cnt_age, k=c(", k_tensor_setting[1], ",", k_tensor_setting[2], "), bs=\"", bs_numeric_setting, "\")"),
      "interaction(part_ethnicity, cnt_ethnicity)",
      sep = " + "
  )
  simple_formula_eth <- as.formula(simple_formula_eth_str)
  message("Complex formula (Age x Eth): ", complex_formula_eth_str)
  message("Simple formula (Age x Eth):  ", simple_formula_eth_str)

  # 3. Fit Models Directly
  complex_gam_eth <- tryCatch({
      mgcv::gam(
          formula = complex_formula_eth,
          data = gam_data_agg_direct_eth,
          family = gam_family_setting,
          method = "REML"
      )
  }, error = function(e) { message("Error fitting complex model (Age x Eth): ", e$message); NULL })

  simple_gam_eth <- tryCatch({
      mgcv::gam(
          formula = simple_formula_eth,
          data = gam_data_agg_direct_eth,
          family = gam_family_setting,
          method = "REML"
      )
  }, error = function(e) { message("Error fitting simple model (Age x Eth): ", e$message); NULL })

  # 4. Compare AIC
  if (!is.null(complex_gam_eth) && !is.null(simple_gam_eth)) {
      aic_complex_eth <- AIC(complex_gam_eth)
      aic_simple_eth <- AIC(simple_gam_eth)
      cat("\n--- AIC Comparison (Age x Ethnicity) ---\n")
      cat(sprintf("AIC (Complex Age-Eth Interaction Model): %.2f\n", aic_complex_eth))
      cat(sprintf("AIC (Simple Age-Eth Model):            %.2f\n", aic_simple_eth))
      if (aic_complex_eth < aic_simple_eth) {
          cat("The complex model with age-ethnicity interactions provides a better fit based on AIC.\n")
      } else if (aic_simple_eth < aic_complex_eth) {
          cat("The simpler model provides a better fit based on AIC.\n")
      } else {
          cat("Both models have similar AIC values.\n")
      }
      # Optional: Add ANOVA comparison
      # anova_comp_eth <- anova(simple_gam_eth, complex_gam_eth, test = "Chisq")
      # print(anova_comp_eth)
  } else {
      message("Could not fit both models for Age x Ethnicity, skipping AIC comparison.")
  }

} else {
  message("\nAge x Ethnicity GAM analysis failed. No results to process or compare.")
}

# --- Analysis 2: Age x Socioeconomic Status (SES) ---
message("\n--- Starting Analysis 2: Age x SES ---")

# Define dimensions for Age x SES
dimensions_age_ses <- c("part_age", "cnt_age", "part_ses", "cnt_ses")

# !!! USER ACTION REQUIRED !!!
# Define breaks for Age x SES
# Adjust age breaks and SES levels as needed for your data
dim_breaks_age_ses <- list(
  part_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  cnt_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  part_ses = c("Low", "Medium", "High"), # Replace with actual factor levels
  cnt_ses = c("Low", "Medium", "High")  # Replace with actual factor levels
)

# Run gam_contact_matrix for Age x SES
gam_results_age_ses <- tryCatch({
  gam_contact_matrix(
    survey = new_survey_data,
    countries = target_country,
    dimensions = dimensions_age_ses,
    dim_breaks = dim_breaks_age_ses,
    family = gam_family_setting,
    age_limits = analysis_age_limits,
    k_tensor = k_tensor_setting,
    k_by = k_by_setting,
    bs_numeric = bs_numeric_setting
  )
}, error = function(e) {
  message("Error running gam_contact_matrix for Age x SES: ", e$message)
  return(NULL)
})

# --- Process and Visualize Age x SES Results ---
if (!is.null(gam_results_age_ses)) {
  message("Age x SES GAM analysis complete. Processing results...")

  # Extract matrix and prediction grid
  predicted_matrix_age_ses <- gam_results_age_ses$matrix
  plot_data_age_ses <- gam_results_age_ses$prediction_grid
  plot_data_age_ses$predicted_contacts <- as.vector(predicted_matrix_age_ses)
  setDT(plot_data_age_ses)

  cat("\nDimensions of the Age x SES predicted matrix:\n")
  print(dim(predicted_matrix_age_ses))
  print(dimnames(predicted_matrix_age_ses))

  # Generate age labels (reusing from above if breaks are the same)
  # Factorise age groups for plotting (reusing from above if breaks are the same)
  plot_data_age_ses[, part_age_group := factor(paste0("[", findInterval(part_age, dim_breaks_age_ses$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dim_breaks_age_ses$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data_age_ses[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dim_breaks_age_ses$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dim_breaks_age_ses$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]

  # --- Visualization (Example: Facet wrap by SES pair) ---
  message("\nCreating Age x SES visualisations...")
  # Ensure factor levels match dim_breaks for faceting labels
  ses_levels <- dim_breaks_age_ses$part_ses
  plot_data_age_ses[, part_ses := factor(part_ses, levels = ses_levels)]
  plot_data_age_ses[, cnt_ses := factor(cnt_ses, levels = ses_levels)]

  p_age_ses <- ggplot(plot_data_age_ses, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts") +
    facet_grid(cnt_ses ~ part_ses, labeller = label_both) +
    labs(
      title = "Predicted Contacts: Age x SES",
      subtitle = paste(ifelse(!is.null(target_country), paste("Country:", target_country), "All Countries"),
                     "| Model: GAM (Age-Age Tensor + SES Interactions + Age-SES Smooths)"),
      x = "Participant Age Group",
      y = "Contact Age Group"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(size = 8, face = "bold")
    )

  # --- Save Age x SES Outputs ---
  output_dir_age_ses <- "output/gam_analysis/age_ses"
  dir.create(output_dir_age_ses, recursive = TRUE, showWarnings = FALSE)

  plot_filename_base_age_ses <- file.path(output_dir_age_ses, paste0("gam_contact_matrix_age_ses", ifelse(!is.null(target_country), paste0("_", target_country), "")))
  ggsave(paste0(plot_filename_base_age_ses, ".png"), plot = p_age_ses, width = 10, height = 8, dpi = 600, bg = "white")
  ggsave(paste0(plot_filename_base_age_ses, ".pdf"), plot = p_age_ses, width = 10, height = 8, dpi = 600, bg = "white")
  message(paste("Age x SES plot saved to:", paste0(plot_filename_base_age_ses, ".png/.pdf")))

  rds_filename_age_ses <- file.path(output_dir_age_ses, paste0("gam_results_age_ses", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".rds"))
  saveRDS(gam_results_age_ses, file = rds_filename_age_ses)
  message(paste("Age x SES GAM results object saved to:", rds_filename_age_ses))

  if (interactive()) print(p_age_ses)

  # --- Compare Models (Age x SES) ---
  message("\nComparing GAM models for Age x SES...")
  # Re-aggregate data and fit models directly for comparison

  # 1. Prepare data (similar to internal steps of gam_contact_matrix)
  data_prep_ses <- copy(new_survey_data)
  # Apply country filter if needed
  if (!is.null(target_country) && "country" %in% names(data_prep_ses$participants)) {
    data_prep_ses$participants <- data_prep_ses$participants[country %in% target_country]
    data_prep_ses$contacts <- data_prep_ses$contacts[part_id %in% data_prep_ses$participants$part_id]
  }
  # Ensure age/SES columns exist (basic check, assumes factors/integers)
  required_dims_ses <- dimensions_age_ses
  # Apply age limits
  if (!is.null(analysis_age_limits)) {
      min_age <- analysis_age_limits[1]
      max_age <- analysis_age_limits[2]
      data_prep_ses$participants <- data_prep_ses$participants[part_age >= min_age & part_age <= max_age]
      data_prep_ses$contacts <- data_prep_ses$contacts[cnt_age >= min_age & cnt_age <= max_age]
      part_ids_remain <- unique(data_prep_ses$participants$part_id)
      data_prep_ses$contacts <- data_prep_ses$contacts[part_id %in% part_ids_remain]
      contact_ids_remain <- unique(data_prep_ses$contacts$part_id)
      data_prep_ses$participants <- data_prep_ses$participants[part_id %in% contact_ids_remain]
  }
  # Merge
  gam_data_full_direct_ses <- merge(
      data_prep_ses$contacts,
      data_prep_ses$participants[, .(part_id, part_age, part_ses)], # Select needed cols
      by = "part_id"
  )
  # Convert factors if needed (should be factors already ideally)
  gam_data_full_direct_ses[, part_ses := as.factor(part_ses)]
  gam_data_full_direct_ses[, cnt_ses := as.factor(cnt_ses)]
  # Aggregate
  gam_data_agg_direct_ses <- gam_data_full_direct_ses[, .N, by = required_dims_ses]
  message("Data prepared for direct model fitting (Age x SES).")

  # 2. Define Formulas
  # Complex formula (matches default gam_contact_matrix behaviour)
  complex_formula_ses_str <- paste(
    paste0("N ~ te(part_age, cnt_age, k = c(", k_tensor_setting[1], ", ", k_tensor_setting[2], "), bs = \"", bs_numeric_setting, "\")"),
    "interaction(part_ses, cnt_ses)",
    paste0("s(part_age, by = interaction(part_ses, cnt_ses), k = ", k_by_setting, ", bs = \"", bs_numeric_setting, "\")"),
    paste0("s(cnt_age, by = interaction(part_ses, cnt_ses), k = ", k_by_setting, ", bs = \"", bs_numeric_setting, "\")"),
    sep = " + "
  )
  complex_formula_ses <- as.formula(complex_formula_ses_str)

  # Simple formula (no age-by-SES smooths)
  simple_formula_ses_str <- paste(
      paste0("N ~ te(part_age, cnt_age, k=c(", k_tensor_setting[1], ",", k_tensor_setting[2], "), bs=\"", bs_numeric_setting, "\")"),
      "interaction(part_ses, cnt_ses)",
      sep = " + "
  )
  simple_formula_ses <- as.formula(simple_formula_ses_str)
  message("Complex formula (Age x SES): ", complex_formula_ses_str)
  message("Simple formula (Age x SES):  ", simple_formula_ses_str)

  # 3. Fit Models Directly
  complex_gam_ses <- tryCatch({
      mgcv::gam(
          formula = complex_formula_ses,
          data = gam_data_agg_direct_ses,
          family = gam_family_setting,
          method = "REML"
      )
  }, error = function(e) { message("Error fitting complex model (Age x SES): ", e$message); NULL })

  simple_gam_ses <- tryCatch({
      mgcv::gam(
          formula = simple_formula_ses,
          data = gam_data_agg_direct_ses,
          family = gam_family_setting,
          method = "REML"
      )
  }, error = function(e) { message("Error fitting simple model (Age x SES): ", e$message); NULL })

  # 4. Compare AIC
  if (!is.null(complex_gam_ses) && !is.null(simple_gam_ses)) {
      aic_complex_ses <- AIC(complex_gam_ses)
      aic_simple_ses <- AIC(simple_gam_ses)
      cat("\n--- AIC Comparison (Age x SES) ---\n")
      cat(sprintf("AIC (Complex Age-SES Interaction Model): %.2f\n", aic_complex_ses))
      cat(sprintf("AIC (Simple Age-SES Model):            %.2f\n", aic_simple_ses))
      if (aic_complex_ses < aic_simple_ses) {
          cat("The complex model with age-SES interactions provides a better fit based on AIC.\n")
      } else if (aic_simple_ses < aic_complex_ses) {
          cat("The simpler model provides a better fit based on AIC.\n")
      } else {
          cat("Both models have similar AIC values.\n")
      }
      # Optional: Add ANOVA comparison
      # anova_comp_ses <- anova(simple_gam_ses, complex_gam_ses, test = "Chisq")
      # print(anova_comp_ses)
  } else {
      message("Could not fit both models for Age x SES, skipping AIC comparison.")
  }

} else {
  message("\nAge x SES GAM analysis failed. No results to process or compare.")
}

message("\n--- Demo script finished ---") 