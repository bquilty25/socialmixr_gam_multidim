# Demo script for gam_contact_matrix with hypothetical new survey data
# Analyzing Age x Ethnicity and Age x Socioeconomic Status (SES) independently

# --- Load Libraries ---
message("Loading required libraries...")
library(socialmixr)
library(mgcv)
library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)

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
survey_raw <- readRDS('data/connect_survey.rds')

# Manually set the class to the expected 'contact_survey'
message("Assigning 'contact_survey' class...")
class(survey_raw) <- c("contact_survey", class(survey_raw))
# Ensure 'list' is also included if it's a list structure
if (is.list(survey_raw) && !"list" %in% class(survey_raw)) {
  class(survey_raw) <- c("contact_survey", "list") # Or adjust based on actual base type
}

new_survey_data <- survey_raw # Rename for consistency

# --- Filter out 'Prefer not to say' Ethnicity ---
message("Filtering out 'Prefer not to say' ethnicity...")
part_filter_level <- "Prefer not to say"
cnt_filter_level <- "Prefer not to say"

# Filter participants
initial_part_rows <- nrow(new_survey_data$participants)
new_survey_data$participants <- new_survey_data$participants[part_ethnicity != part_filter_level]
filtered_part_rows <- nrow(new_survey_data$participants)
message(paste("-> Removed", initial_part_rows - filtered_part_rows, "participants with ethnicity '", part_filter_level, "'."))

# Filter contacts
initial_cnt_rows <- nrow(new_survey_data$contacts)
new_survey_data$contacts <- new_survey_data$contacts[cnt_ethnicity != cnt_filter_level]
filtered_cnt_rows <- nrow(new_survey_data$contacts)
message(paste("-> Removed", initial_cnt_rows - filtered_cnt_rows, "contacts with ethnicity '", cnt_filter_level, "'."))

# --- Filter out 'Prefer not to say' Gender (Contacts Only) ---
message("Filtering out 'Prefer not to say' contact gender...")
part_gender_filter_level <- "Prefer not to say" # Define level
initial_cnt_gender_rows <- nrow(new_survey_data$contacts)
new_survey_data$contacts <- new_survey_data$contacts[cnt_gender != part_gender_filter_level]
filtered_cnt_gender_rows <- nrow(new_survey_data$contacts)
message(paste("-> Removed", initial_cnt_gender_rows - filtered_cnt_gender_rows, "contacts with gender '", part_gender_filter_level, "'."))
# ------------------------------------------------------------

# Ensure consistency: Remove contacts whose participant was filtered out
part_ids_remaining <- unique(new_survey_data$participants$part_id)
contacts_before_consistency <- nrow(new_survey_data$contacts)
new_survey_data$contacts <- new_survey_data$contacts[part_id %in% part_ids_remaining]
contacts_after_consistency <- nrow(new_survey_data$contacts)
if(contacts_after_consistency < contacts_before_consistency) {
    message(paste("-> Removed", contacts_before_consistency - contacts_after_consistency, "contacts due to participant filtering."))
}

# Ensure consistency: Remove participants with no contacts left
contact_ids_remaining <- unique(new_survey_data$contacts$part_id)
participants_before_consistency <- nrow(new_survey_data$participants)
new_survey_data$participants <- new_survey_data$participants[part_id %in% contact_ids_remaining]
participants_after_consistency <- nrow(new_survey_data$participants)
if(participants_after_consistency < participants_before_consistency) {
    message(paste("-> Removed", participants_before_consistency - participants_after_consistency, "participants with no remaining contacts after filtering."))
}
message("Filtering complete.")

levels(new_survey_data$participants$part_ethnicity)
levels(new_survey_data$contacts$cnt_ethnicity)

message("Survey data loaded and class assigned.")

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
k_by_setting <- 6 # Restore default (won't be used by auto-formula now)
bs_numeric_setting <- "ps"

# --- Analysis 1: Age x Ethnicity ---
message("\n--- Starting Analysis 1: Age x Ethnicity ---")

# Define dimensions for Age x Ethnicity
dimensions_age_eth <- c("part_age", "cnt_age", "part_ethnicity", "cnt_ethnicity")

# Get actual ethnicity levels from the *filtered* data
part_eth_levels <- levels(droplevels(new_survey_data$participants$part_ethnicity))
cnt_eth_levels <- levels(droplevels(new_survey_data$contacts$cnt_ethnicity))
message("Using filtered ethnicity levels for breaks:")
message(" Part levels: ", paste(part_eth_levels, collapse=", "))
message(" Cont levels: ", paste(cnt_eth_levels, collapse=", "))

# !!! USER ACTION REQUIRED !!!
# Define breaks for Age x Ethnicity
# Adjust age breaks and ethnicity levels as needed for your data
dim_breaks_age_eth <- list(
  part_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  cnt_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  part_ethnicity = part_eth_levels,
  cnt_ethnicity = cnt_eth_levels
)

# Run gam_contact_matrix for Age x Ethnicity
gam_results_age_eth <- tryCatch({
  # Return value is now the full list again
  gam_contact_matrix(
    survey = new_survey_data,
    countries = target_country, # Use NULL if country filtering not needed
    dimensions = dimensions_age_eth,
    dim_breaks = dim_breaks_age_eth,
    family = gam_family_setting,
    age_limits = analysis_age_limits,
    k_tensor = k_tensor_setting,
    # k_by = k_by_setting, # Not used by simplified auto-formula
    bs_numeric = bs_numeric_setting,
    use_bam = TRUE # Explicitly use bam for ethnicity
  )
}, error = function(e) {
  message("Error running gam_contact_matrix for Age x Ethnicity: ", e$message)
  return(NULL)
})

# --- Process and Visualize Age x Ethnicity Results --- # UNCOMMENTED
if (!is.null(gam_results_age_eth)) {
  message("Age x Ethnicity GAM analysis complete. Processing results...")

  # Extract matrix and prediction grid
  predicted_matrix_age_eth <- gam_results_age_eth$matrix # Now available
  plot_data_age_eth <- gam_results_age_eth$prediction_grid # Now available
  plot_data_age_eth$predicted_contacts <- as.vector(predicted_matrix_age_eth)
  setDT(plot_data_age_eth)
  message("-> Results extracted.")

  cat("\nDimensions of the Age x Ethnicity predicted matrix:\n")
  print(dim(predicted_matrix_age_eth))
  print(dimnames(predicted_matrix_age_eth))

  # Generate age labels from breaks for plotting
  age_labels_part <- paste0("[", dim_breaks_age_eth$part_age[-length(dim_breaks_age_eth$part_age)], ",", dim_breaks_age_eth$part_age[-1], ")")
  age_labels_cnt <- paste0("[", dim_breaks_age_eth$cnt_age[-length(dim_breaks_age_eth$cnt_age)], ",", dim_breaks_age_eth$cnt_age[-1], ")")
  message("-> Age labels generated.")

  # Factorise age groups for plotting
  # Note: This assumes regular 5-year bands based on seq(..., 5)
  plot_data_age_eth[, part_age_group := factor(paste0("[", findInterval(part_age, dim_breaks_age_eth$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dim_breaks_age_eth$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data_age_eth[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dim_breaks_age_eth$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dim_breaks_age_eth$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]
  message("-> Age groups factorised.")

  # --- Visualization (Example: Facet wrap by ethnicity pair) --- 
  message("\nCreating Age x Ethnicity visualisations...")
  # Ensure factor levels match dim_breaks for faceting labels
  eth_levels <- dim_breaks_age_eth$part_ethnicity
  plot_data_age_eth[, part_ethnicity := factor(part_ethnicity, levels = eth_levels)]
  plot_data_age_eth[, cnt_ethnicity := factor(cnt_ethnicity, levels = eth_levels)]

  message("-> Creating p_age_eth plot object...")
  p_age_eth <- ggplot(plot_data_age_eth, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts", 
                         na.value = "grey80", trans = scales::log10_trans()) + # Use log10 trans
    facet_grid(cnt_ethnicity ~ part_ethnicity, labeller = labeller(.default = label_both, .multi_line = TRUE)) +
    labs(
      title = "Predicted Contacts: Age x Ethnicity",
      subtitle = paste(ifelse(!is.null(target_country), paste("Country:", target_country), "All Countries"),
                     "| Model: BAM (Age-Age Tensor + Eth Interaction)"), # Simplified model name
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
  message("-> p_age_eth plot object created.")

  # --- Save Age x Ethnicity Outputs --- 
  output_dir_age_eth <- "output/gam_analysis/age_ethnicity"
  dir.create(output_dir_age_eth, recursive = TRUE, showWarnings = FALSE)
  
  message("-> Preparing data for flattened_matrix...")
  plot_data_age_eth_flat <- copy(plot_data_age_eth)
  plot_data_age_eth_flat <- arrange(plot_data_age_eth_flat, part_age, cnt_age)
  # Ensure factor levels are ordered correctly for faceting
  plot_data_age_eth_flat$part_age_group <- factor(plot_data_age_eth_flat$part_age_group, levels = age_labels_part) 
  plot_data_age_eth_flat$cnt_age_group <- factor(plot_data_age_eth_flat$cnt_age_group, levels = age_labels_cnt)
  # Reverse the factor levels for contact age group to flip facet order
  plot_data_age_eth_flat$cnt_age_group <- factor(plot_data_age_eth_flat$cnt_age_group, levels = rev(age_labels_cnt))
  
  message("-> Creating flattened_matrix plot object...")
  p_age_eth_flattened <- ggplot(plot_data_age_eth_flat, # Rename plot object
                             aes(x = part_ethnicity, y = cnt_ethnicity, fill = predicted_contacts)) + 
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Mean contacts", 
                         na.value="grey80", trans = scales::log10_trans()) + # Use log10 trans
    labs(
      title = sprintf("Mean Contacts: Age Group x Ethnicity"),
      subtitle = paste("Model: BAM (Tensor Spline + Ethnicity Interaction)"),
      x = "Participant Ethnicity",
      y = "Contact Ethnicity"
    ) +
    theme_minimal(base_size = 10) +
    facet_grid(cnt_age_group ~ part_age_group, switch = 'both', 
               labeller = labeller(.multi_line = TRUE)) + # Allow multiline facet labels
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
  message("-> p_age_eth_flattened plot object created.") # Update message

  message("-> Saving p_age_eth_flattened plot...") # Update message
  # Consistent filename for the flattened plot
  plot_filename_flat_age_eth <- file.path(output_dir_age_eth, paste0("gam_contact_matrix_age_ethnicity_flattened", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".png"))
  ggsave(plot_filename_flat_age_eth, plot = p_age_eth_flattened, width = 12, height = 10, dpi = 600, bg="white")
  # Also save as PDF for consistency with other main plots
  plot_filename_flat_age_eth_pdf <- sub(".png$", ".pdf", plot_filename_flat_age_eth)
  ggsave(plot_filename_flat_age_eth_pdf, plot = p_age_eth_flattened, width = 12, height = 10, dpi = 600, bg="white")
  message(paste("-> Flattened Age x Ethnicity plot saved to:", plot_filename_flat_age_eth, "and .pdf")) # Update message

  plot_filename_base_age_eth <- file.path(output_dir_age_eth, paste0("gam_contact_matrix_age_ethnicity", ifelse(!is.null(target_country), paste0("_", target_country), "")))
  message("-> Saving p_age_eth plots...")
  ggsave(paste0(plot_filename_base_age_eth, ".png"), plot = p_age_eth, width = 10, height = 8, dpi = 600, bg = "white")
  ggsave(paste0(plot_filename_base_age_eth, ".pdf"), plot = p_age_eth, width = 10, height = 8, dpi = 600, bg = "white")
  message(paste("-> Age x Ethnicity plot saved to:", paste0(plot_filename_base_age_eth, ".png/.pdf")))

  rds_filename_age_eth <- file.path(output_dir_age_eth, paste0("gam_results_age_ethnicity", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".rds"))
  message("-> Saving RDS object...")
  saveRDS(gam_results_age_eth, file = rds_filename_age_eth) # Restore saving
  message(paste("-> Age x Ethnicity GAM results object saved to:", rds_filename_age_eth))

  # if (interactive()) print(p_age_eth) # Keep commented out for non-interactive run

  #   # --- Compare Models (Age x Ethnicity) --- #
  message("\nComparing GAM models for Age x Ethnicity...")
  # Fit a simpler model without the ethnicity interaction for comparison
  # Note: Requires gam_formula = NULL in gam_contact_matrix to be modified or
  # a separate function call that omits the interaction term build logic.
  # FOR NOW: We assume gam_contact_matrix needs to be called again with
  # appropriate dimensions/formula if we want a true comparison.
  # Simplified approach: Manually define a simpler formula
  # (This requires gam_results_age_eth to contain the fitted model `gam_fit`)

  # Example: Fit model without the explicit ethnicity interaction term
  # Get the formula used for the main model (assuming it's stored)
  main_formula_eth_str <- deparse(gam_results_age_eth$gam_formula)
  # Attempt to create a simpler formula string (may need refinement)
  simple_formula_eth_str <- gsub("\\s*\\+\\s*ethnicity_interaction", "", main_formula_eth_str)

  if (simple_formula_eth_str != main_formula_eth_str) {
    simple_formula_eth <- as.formula(simple_formula_eth_str)
    message("Fitting simpler Age x Ethnicity model with formula: ", simple_formula_eth_str)

    gam_results_simple_eth <- tryCatch({
      # Re-run using the simpler formula - ensure data prep is implicitly handled
      # or pass the aggregated data if gam_contact_matrix doesn't expose it
      # This assumes gam_contact_matrix can accept a formula override
      # or we fit directly using bam() on prepared data.
      # Let's try fitting directly for simplicity here:
      simpler_model_eth <- mgcv::bam(
          formula = simple_formula_eth,
          data = gam_results_age_eth$fitting_data, # Use fitting data returned by function
          family = gam_family_setting,
          method = "fREML",
          discrete = TRUE
          # Add other relevant bam args if needed
      )
      message("Simpler model fitted.")
      list(gam_fit = simpler_model_eth) # Mimic structure
    }, error = function(e) {
       message("Error fitting simpler Age x Ethnicity model: ", e$message)
       return(NULL)
    })

    if (!is.null(gam_results_simple_eth)) {
      # Compare AIC
      aic_full_eth <- AIC(gam_results_age_eth$gam_fit)
      aic_simple_eth <- AIC(gam_results_simple_eth$gam_fit)
      message(paste("AIC (Full Age x Eth Model):", round(aic_full_eth, 2)))
      message(paste("AIC (Simple Age x Eth Model):", round(aic_simple_eth, 2)))
      if (aic_full_eth < aic_simple_eth) {
        message("Full model (with ethnicity interaction) has lower AIC.")
      } else {
        message("Simpler model (without ethnicity interaction) has lower AIC.")
      }
    } else {
       message("Could not fit simpler model for Age x Ethnicity, skipping AIC comparison.")
    }
  } else {
      message("Could not derive simpler formula for Age x Ethnicity, skipping comparison.")
  }
  # --- End Comparison --- #

} else {
  message("\nAge x Ethnicity GAM analysis failed. No results to process or compare.")
}

# --- Analysis 2: Age x Socioeconomic Status (SES) --- # UNCOMMENTING
message("\n--- Starting Analysis 2: Age x SES ---")

# Define dimensions for Age x SES
dimensions_age_ses <- c("part_age", "cnt_age", "part_ses", "cnt_ses")

# !!! USER ACTION REQUIRED !!!
# Define breaks for Age x SES
# Adjust age breaks and SES levels as needed for your data
dim_breaks_age_ses <- list(
  part_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  cnt_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  part_ses = levels(new_survey_data$participants$part_ses),
  cnt_ses = levels(new_survey_data$contacts$cnt_ses) # Corrected: use contacts data
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
    # k_by = k_by_setting, # Not used by simplified auto-formula
    bs_numeric = bs_numeric_setting,
    use_bam = TRUE # Use bam for SES now
  )
}, error = function(e) {
  message("Error running gam_contact_matrix for Age x SES: ", e$message)
  return(NULL)
})

# --- Process and Visualize Age x SES Results --- # ADDING MESSAGES
if (!is.null(gam_results_age_ses)) {
  message("Age x SES GAM analysis complete. Processing results...")

  # Extract matrix and prediction grid
  predicted_matrix_age_ses <- gam_results_age_ses$matrix
  plot_data_age_ses <- gam_results_age_ses$prediction_grid
  plot_data_age_ses$predicted_contacts <- as.vector(predicted_matrix_age_ses)
  setDT(plot_data_age_ses)
  message("-> SES Results extracted.")

  cat("\nDimensions of the Age x SES predicted matrix:\n")
  print(dim(predicted_matrix_age_ses))
  print(dimnames(predicted_matrix_age_ses))

  # Generate age labels (reusing from above if breaks are the same)
  message("-> SES Age labels reused.")
  # Factorise age groups for plotting (reusing from above if breaks are the same)
  plot_data_age_ses[, part_age_group := factor(paste0("[", findInterval(part_age, dim_breaks_age_ses$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dim_breaks_age_ses$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data_age_ses[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dim_breaks_age_ses$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dim_breaks_age_ses$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]
  message("-> SES Age groups factorised.")

  # --- Visualization (Example: Facet wrap by SES pair) ---
  message("\nCreating Age x SES visualisations...")
  # Ensure factor levels match dim_breaks for faceting labels
  ses_levels <- dim_breaks_age_ses$part_ses
  plot_data_age_ses[, part_ses := factor(part_ses, levels = ses_levels)]
  plot_data_age_ses[, cnt_ses := factor(cnt_ses, levels = dim_breaks_age_ses$cnt_ses)] # Use cnt_ses levels

  message("-> Creating p_age_ses plot object...")
  p_age_ses <- ggplot(plot_data_age_ses, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts", 
                         na.value = "grey80", trans = scales::log10_trans()) + # Use log10 trans
    facet_grid(cnt_ses ~ part_ses, labeller = labeller(.default = label_both, .multi_line = TRUE)) +
    labs(
      title = "Predicted Contacts: Age x SES",
      subtitle = paste(ifelse(!is.null(target_country), paste("Country:", target_country), "All Countries"),
                     "| Model: BAM (Age-Age Tensor + SES Interaction)"), # Updated model name
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
  message("-> p_age_ses plot object created.")

  # --- Save Age x SES Outputs --- 
  output_dir_age_ses <- "output/gam_analysis/age_ses"
  dir.create(output_dir_age_ses, recursive = TRUE, showWarnings = FALSE)

  # --- Create and Save Flattened SES plot ---
  message("-> Preparing data for flattened SES matrix...")
  plot_data_age_ses_flat <- copy(plot_data_age_ses)
  # Ensure factor levels are ordered correctly for faceting
  plot_data_age_ses_flat$part_age_group <- factor(plot_data_age_ses_flat$part_age_group, levels = age_labels_part)
  plot_data_age_ses_flat$cnt_age_group <- factor(plot_data_age_ses_flat$cnt_age_group, levels = age_labels_cnt)
  # Reverse the factor levels for contact age group to flip facet order
  plot_data_age_ses_flat$cnt_age_group <- factor(plot_data_age_ses_flat$cnt_age_group, levels = rev(age_labels_cnt))

  message("-> Creating p_age_ses_flattened plot object...")
  p_age_ses_flattened <- ggplot(plot_data_age_ses_flat, 
                                aes(x = part_ses, y = cnt_ses, fill = predicted_contacts)) + 
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Mean contacts", 
                         na.value="grey80", trans = scales::log10_trans()) + # Use log10 trans
    labs(
      title = sprintf("Mean Contacts: Age Group x SES"),
      subtitle = paste("Model: BAM (Tensor Spline + SES Interaction)"),
      x = "Participant SES",
      y = "Contact SES"
    ) +
    theme_minimal(base_size = 10) +
    facet_grid(cnt_age_group ~ part_age_group, switch = 'both') + 
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
  message("-> p_age_ses_flattened plot object created.")
  
  message("-> Saving p_age_ses_flattened plot...")
  plot_filename_flat_age_ses <- file.path(output_dir_age_ses, paste0("gam_contact_matrix_age_ses_flattened", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".png"))
  ggsave(plot_filename_flat_age_ses, plot = p_age_ses_flattened, width = 12, height = 10, dpi = 600, bg="white")
  plot_filename_flat_age_ses_pdf <- sub(".png$", ".pdf", plot_filename_flat_age_ses)
  ggsave(plot_filename_flat_age_ses_pdf, plot = p_age_ses_flattened, width = 12, height = 10, dpi = 600, bg="white")
  message(paste("-> Flattened Age x SES plot saved to:", plot_filename_flat_age_ses, "and .pdf"))
  # --------------------------------------

  plot_filename_base_age_ses <- file.path(output_dir_age_ses, paste0("gam_contact_matrix_age_ses", ifelse(!is.null(target_country), paste0("_", target_country), "")))
  message("-> Saving p_age_ses plots...")
  ggsave(paste0(plot_filename_base_age_ses, ".png"), plot = p_age_ses, width = 10, height = 8, dpi = 600, bg = "white")
  ggsave(paste0(plot_filename_base_age_ses, ".pdf"), plot = p_age_ses, width = 10, height = 8, dpi = 600, bg = "white")
  message(paste("-> Age x SES plot saved to:", paste0(plot_filename_base_age_ses, ".png/.pdf")))

  rds_filename_age_ses <- file.path(output_dir_age_ses, paste0("gam_results_age_ses", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".rds"))
  message("-> Saving SES RDS object...")
  saveRDS(gam_results_age_ses, file = rds_filename_age_ses)
  message(paste("-> Age x SES GAM results object saved to:", rds_filename_age_ses))

  # if (interactive()) print(p_age_ses)

  #   # --- Compare Models (Age x SES) --- #
  message("\nComparing GAM models for Age x SES...")
  # Similar logic as for Ethnicity
  main_formula_ses_str <- deparse(gam_results_age_ses$gam_formula)
  simple_formula_ses_str <- gsub("\\s*\\+\\s*ses_interaction", "", main_formula_ses_str)

  if (simple_formula_ses_str != main_formula_ses_str) {
      simple_formula_ses <- as.formula(simple_formula_ses_str)
      message("Fitting simpler Age x SES model with formula: ", simple_formula_ses_str)

      gam_results_simple_ses <- tryCatch({
          simpler_model_ses <- mgcv::bam(
              formula = simple_formula_ses,
              data = gam_results_age_ses$fitting_data, # Use fitting data returned by function
              family = gam_family_setting,
              method = "fREML",
              discrete = TRUE
          )
          message("Simpler model fitted.")
          list(gam_fit = simpler_model_ses)
      }, error = function(e) {
          message("Error fitting simpler Age x SES model: ", e$message)
          return(NULL)
      })

      if (!is.null(gam_results_simple_ses)) {
          aic_full_ses <- AIC(gam_results_age_ses$gam_fit)
          aic_simple_ses <- AIC(gam_results_simple_ses$gam_fit)
          message(paste("AIC (Full Age x SES Model):", round(aic_full_ses, 2)))
          message(paste("AIC (Simple Age x SES Model):", round(aic_simple_ses, 2)))
          if (aic_full_ses < aic_simple_ses) {
              message("Full model (with SES interaction) has lower AIC.")
          } else {
              message("Simpler model (without SES interaction) has lower AIC.")
          }
      } else {
          message("Could not fit simpler model for Age x SES, skipping AIC comparison.")
      }
  } else {
      message("Could not derive simpler formula for Age x SES, skipping comparison.")
  }
  # --- End Comparison --- #

} else {
  message("\nAge x SES GAM analysis failed. No results to process or compare.")
}

# --- Analysis 3: Age x Gender --- # ADDING NEW ANALYSIS
message("\n--- Starting Analysis 3: Age x Gender ---")

# Define dimensions for Age x Gender
dimensions_age_gender <- c("part_age", "cnt_age", "part_gender", "cnt_gender")

# !!! MOVED: Define breaks AFTER filtering !!!
# Get actual gender levels from the *filtered* data
part_gender_levels <- levels(droplevels(new_survey_data$participants$part_gender))
cnt_gender_levels <- levels(droplevels(new_survey_data$contacts$cnt_gender)) # Use filtered contacts gender
message("Using filtered gender levels for breaks:")
message(" Part levels: ", paste(part_gender_levels, collapse=", "))
message(" Cont levels: ", paste(cnt_gender_levels, collapse=", "))

dim_breaks_age_gender <- list(
  part_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  cnt_age = seq(analysis_age_limits[1], analysis_age_limits[2], 5),
  part_gender = part_gender_levels,
  cnt_gender = cnt_gender_levels
)
# !!! ------------------------------------ !!!

# Run gam_contact_matrix for Age x Gender
gam_results_age_gender <- tryCatch({
  gam_contact_matrix(
    survey = new_survey_data,
    countries = target_country,
    dimensions = dimensions_age_gender,
    dim_breaks = dim_breaks_age_gender,
    family = gam_family_setting,
    age_limits = analysis_age_limits,
    k_tensor = k_tensor_setting,
    bs_numeric = bs_numeric_setting,
    use_bam = TRUE # Use bam for gender
  )
}, error = function(e) {
  message("Error running gam_contact_matrix for Age x Gender: ", e$message)
  return(NULL)
})

# --- Process and Visualize Age x Gender Results ---
if (!is.null(gam_results_age_gender)) {
  message("Age x Gender GAM analysis complete. Processing results...")

  # Extract matrix and prediction grid
  predicted_matrix_age_gender <- gam_results_age_gender$matrix
  plot_data_age_gender <- gam_results_age_gender$prediction_grid
  plot_data_age_gender$predicted_contacts <- as.vector(predicted_matrix_age_gender)
  setDT(plot_data_age_gender)
  message("-> Gender Results extracted.")

  cat("\nDimensions of the Age x Gender predicted matrix:\n")
  print(dim(predicted_matrix_age_gender))
  print(dimnames(predicted_matrix_age_gender))

  # Generate age labels (reusing from above if breaks are the same)
  message("-> Gender Age labels reused.")
  # Factorise age groups for plotting (reusing from above if breaks are the same)
  plot_data_age_gender[, part_age_group := factor(paste0("[", findInterval(part_age, dim_breaks_age_gender$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dim_breaks_age_gender$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data_age_gender[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dim_breaks_age_gender$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dim_breaks_age_gender$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]
  message("-> Gender Age groups factorised.")

  # --- Visualization (Example: Facet wrap by Gender pair) ---
  message("\nCreating Age x Gender visualisations...")
  # Ensure factor levels match dim_breaks for faceting labels
  gender_levels_part <- dim_breaks_age_gender$part_gender
  gender_levels_cnt <- dim_breaks_age_gender$cnt_gender
  plot_data_age_gender[, part_gender := factor(part_gender, levels = gender_levels_part)]
  plot_data_age_gender[, cnt_gender := factor(cnt_gender, levels = gender_levels_cnt)]

  message("-> Creating p_age_gender plot object...")
  p_age_gender <- ggplot(plot_data_age_gender, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts",
                         na.value = "grey80", trans = scales::log10_trans()) + # Use log10 trans
    facet_grid(cnt_gender ~ part_gender, labeller = labeller(.default = label_both, .multi_line = TRUE)) +
    labs(
      title = "Predicted Contacts: Age x Gender",
      subtitle = paste(ifelse(!is.null(target_country), paste("Country:", target_country), "All Countries"),
                     "| Model: BAM (Age-Age Tensor + Gender Interaction)"), # Updated model name
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
  message("-> p_age_gender plot object created.")

  # --- Save Age x Gender Outputs ---
  output_dir_age_gender <- "output/gam_analysis/age_gender"
  dir.create(output_dir_age_gender, recursive = TRUE, showWarnings = FALSE)

  # --- Create and Save Flattened Gender plot ---
  message("-> Preparing data for flattened Gender matrix...")
  plot_data_age_gender_flat <- copy(plot_data_age_gender)
  # Ensure factor levels are ordered correctly for faceting
  plot_data_age_gender_flat$part_age_group <- factor(plot_data_age_gender_flat$part_age_group, levels = age_labels_part)
  plot_data_age_gender_flat$cnt_age_group <- factor(plot_data_age_gender_flat$cnt_age_group, levels = age_labels_cnt)
  # Reverse the factor levels for contact age group to flip facet order
  plot_data_age_gender_flat$cnt_age_group <- factor(plot_data_age_gender_flat$cnt_age_group, levels = rev(age_labels_cnt))

  message("-> Creating p_age_gender_flattened plot object...")
  p_age_gender_flattened <- ggplot(plot_data_age_gender_flat,
                                aes(x = part_gender, y = cnt_gender, fill = predicted_contacts)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Mean contacts",
                         na.value="grey80", trans = scales::log10_trans()) + # Use log10 trans
    labs(
      title = sprintf("Mean Contacts: Age Group x Gender"),
      subtitle = paste("Model: BAM (Tensor Spline + Gender Interaction)"),
      x = "Participant Gender",
      y = "Contact Gender"
    ) +
    theme_minimal(base_size = 10) +
    facet_grid(cnt_age_group ~ part_age_group, switch = 'both') +
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
  message("-> p_age_gender_flattened plot object created.")

  message("-> Saving p_age_gender_flattened plot...")
  plot_filename_flat_age_gender <- file.path(output_dir_age_gender, paste0("gam_contact_matrix_age_gender_flattened", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".png"))
  ggsave(plot_filename_flat_age_gender, plot = p_age_gender_flattened, width = 12, height = 10, dpi = 600, bg="white")
  plot_filename_flat_age_gender_pdf <- sub(".png$", ".pdf", plot_filename_flat_age_gender)
  ggsave(plot_filename_flat_age_gender_pdf, plot = p_age_gender_flattened, width = 12, height = 10, dpi = 600, bg="white")
  message(paste("-> Flattened Age x Gender plot saved to:", plot_filename_flat_age_gender, "and .pdf"))
  # --------------------------------------

  plot_filename_base_age_gender <- file.path(output_dir_age_gender, paste0("gam_contact_matrix_age_gender", ifelse(!is.null(target_country), paste0("_", target_country), "")))
  message("-> Saving p_age_gender plots...")
  ggsave(paste0(plot_filename_base_age_gender, ".png"), plot = p_age_gender, width = 10, height = 8, dpi = 600, bg = "white")
  ggsave(paste0(plot_filename_base_age_gender, ".pdf"), plot = p_age_gender, width = 10, height = 8, dpi = 600, bg = "white")
  message(paste("-> Age x Gender plot saved to:", paste0(plot_filename_base_age_gender, ".png/.pdf")))

  rds_filename_age_gender <- file.path(output_dir_age_gender, paste0("gam_results_age_gender", ifelse(!is.null(target_country), paste0("_", target_country), ""), ".rds"))
  message("-> Saving Gender RDS object...")
  saveRDS(gam_results_age_gender, file = rds_filename_age_gender)
  message(paste("-> Age x Gender GAM results object saved to:", rds_filename_age_gender))

  # --- Compare Models (Age x Gender) --- #
  message("\nComparing GAM models for Age x Gender...")
  # Similar logic as for Ethnicity/SES
  main_formula_gender_str <- deparse(gam_results_age_gender$gam_formula)
  # Note: Assumes the interaction column is named 'gender_interaction'
  simple_formula_gender_str <- gsub("\\s*\\+\\s*gender_interaction", "", main_formula_gender_str)

  if (simple_formula_gender_str != main_formula_gender_str) {
      simple_formula_gender <- as.formula(simple_formula_gender_str)
      message("Fitting simpler Age x Gender model with formula: ", simple_formula_gender_str)

      gam_results_simple_gender <- tryCatch({
          simpler_model_gender <- mgcv::bam(
              formula = simple_formula_gender,
              data = gam_results_age_gender$fitting_data, # Use fitting data returned by function
              family = gam_family_setting,
              method = "fREML",
              discrete = TRUE
          )
          message("Simpler model fitted.")
          list(gam_fit = simpler_model_gender)
      }, error = function(e) {
          message("Error fitting simpler Age x Gender model: ", e$message)
          return(NULL)
      })

      if (!is.null(gam_results_simple_gender)) {
          aic_full_gender <- AIC(gam_results_age_gender$gam_fit)
          aic_simple_gender <- AIC(gam_results_simple_gender$gam_fit)
          message(paste("AIC (Full Age x Gender Model):", round(aic_full_gender, 2)))
          message(paste("AIC (Simple Age x Gender Model):", round(aic_simple_gender, 2)))
          if (aic_full_gender < aic_simple_gender) {
              message("Full model (with Gender interaction) has lower AIC.")
          } else {
              message("Simpler model (without Gender interaction) has lower AIC.")
          }
      } else {
          message("Could not fit simpler model for Age x Gender, skipping AIC comparison.")
      }
  } else {
      message("Could not derive simpler formula for Age x Gender, skipping comparison.")
  }
  # --- End Comparison --- #

} else {
  message("\nAge x Gender GAM analysis failed. No results to process or compare.")
}

message("\n--- Demo script finished ---") 