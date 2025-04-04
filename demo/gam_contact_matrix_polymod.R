# Demo script for gam_contact_matrix with polymod data (Age and Gender)

# Load necessary libraries
library(socialmixr)
library(mgcv)
library(data.table)
library(ggplot2)
library(patchwork)

# Load polymod data
data(polymod)

# --- Configuration ---
# Define dimensions for the contact matrix
dimensions <- c("part_age", "cnt_age", "part_gender", "cnt_gender")

# Define breaks for age (e.g., 5-year bands up to 75) and gender levels
# Ensure gender levels match those in the polymod data (usually 'F' and 'M')
dimension_breaks <- list(
  part_age = seq(0, 75, 5),
  cnt_age = seq(0, 75, 5),
  part_gender = c("F", "M"),
  cnt_gender = c("F", "M")
)

# Define the GAM formula -- NO LONGER NEEDED, will be built internally
# gam_formula <- N ~ te(part_age, cnt_age, k = c(10, 10), bs = "ps") +
#                    s(part_age, bs="ps", k = 10) + s(cnt_age, bs="ps", k = 10) + # Main effects for age
#                    interaction(part_gender, cnt_gender)

# Specify the country
target_country <- "United Kingdom"

# Specify age limits for the analysis
analysis_age_limits <- c(0, 75)

# --- Run gam_contact_matrix ---
message("Running gam_contact_matrix function (default includes age-gender interaction)...")

# Ensure the gam_contact_matrix function is loaded (if running interactively or sourced)
source("R/contact_matrix.R") # Load relative to workspace root

# The default formula now includes age-gender interactions.
# If you wanted a simpler model (e.g., separate age and gender effects),
# you would need to define it manually and pass it via the `gam_formula` argument:
# gam_formula_simple <- N ~ te(part_age, cnt_age, k=c(10,10), bs="ps") +
#                         s(part_age, k=10, bs="ps") + s(cnt_age, k=10, bs="ps") +
#                         interaction(part_gender, cnt_gender)

gam_results <- tryCatch({
  gam_contact_matrix(
    survey = polymod,
    countries = target_country,
    # gam_formula = NULL, # Use default formula generation
    # gam_formula = gam_formula_simple, # Uncomment to use the simpler formula defined above
    dimensions = dimensions,
    dim_breaks = dimension_breaks,
    family = nb(),
    age_limits = analysis_age_limits
    # Optional arguments to control smooth complexity (using defaults here):
    # k_tensor = c(8, 8),
    # k_by = 6,
    # bs_numeric = "ps"
  )
}, error = function(e) {
  message("Error running gam_contact_matrix: ", e$message)
  return(NULL)
})

# --- Process and Visualise Results ---
if (!is.null(gam_results)) {
  message("GAM analysis complete. Processing results...")

  # Extract the predicted matrix (multidimensional array)
  predicted_matrix <- gam_results$matrix
  cat("\nDimensions of the predicted contact matrix:\n")
  print(dim(predicted_matrix))
  print(dimnames(predicted_matrix))

  # Example: Print the predicted contacts for 20-24 yr old Females with 30-34 yr old Males
  # Ensure dimnames match the structure
  if (all(c("part_age", "cnt_age", "part_gender", "cnt_gender") %in% names(dimnames(predicted_matrix)))) {
      pred_val <- predicted_matrix["[20,25)", "[30,35)", "F", "M"]
      cat(sprintf("\nPredicted contacts (F [20,25) -> M [30,35)): %.3f\n", pred_val))
  }

  # --- Visualisation using ggplot2 ---
  message("\nCreating visualisations...")

  # Convert the prediction grid and predictions into a data.table for plotting
  plot_data <- gam_results$prediction_grid
  plot_data$predicted_contacts <- as.vector(predicted_matrix)
  setDT(plot_data)

  # Generate age labels from breaks for plotting
  age_labels_part <- paste0("[", dimension_breaks$part_age[-length(dimension_breaks$part_age)], ",", dimension_breaks$part_age[-1], ")")
  age_labels_cnt <- paste0("[", dimension_breaks$cnt_age[-length(dimension_breaks$cnt_age)], ",", dimension_breaks$cnt_age[-1], ")")

  # Factorise for correct ordering in ggplot
  plot_data[, part_age_group := factor(paste0("[", findInterval(part_age, dimension_breaks$part_age, left.open = TRUE) * 5 - 5, ",", findInterval(part_age, dimension_breaks$part_age, left.open = TRUE) * 5, ")"), levels = age_labels_part)]
  plot_data[, cnt_age_group := factor(paste0("[", findInterval(cnt_age, dimension_breaks$cnt_age, left.open = TRUE) * 5 - 5, ",", findInterval(cnt_age, dimension_breaks$cnt_age, left.open = TRUE) * 5, ")"), levels = age_labels_cnt)]

  # Create a list to store plots
  plots_list <- list()

  # Loop through each gender combination for visualisation
  gender_pairs <- expand.grid(part_g = dimension_breaks$part_gender, cnt_g = dimension_breaks$cnt_gender)

  for (i in 1:nrow(gender_pairs)) {
    p_gender <- as.character(gender_pairs$part_g[i])
    c_gender <- as.character(gender_pairs$cnt_g[i])

    plot_subset <- plot_data[part_gender == p_gender & cnt_gender == c_gender]

    p <- ggplot(plot_subset, aes(x = part_age_group, y = cnt_age_group, fill = predicted_contacts)) +
      geom_tile(colour = "white", linewidth = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Predicted\nContacts", limits = c(0, max(plot_data$predicted_contacts, na.rm = TRUE))) +
      labs(
        title = sprintf("Predicted Contacts: %s Participants vs %s Contacts", p_gender, c_gender),
        subtitle = paste("Country:", target_country, "| Model: GAM (Tensor Spline + Gender Interaction)"),
        x = "Participant Age Group",
        y = "Contact Age Group"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=7),
        axis.text.y = element_text(size=7),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9)
      )

    plots_list[[paste(p_gender, c_gender, sep = "_")]] <- p

  }
  
  plot_data <- arrange(plot_data, part_age, cnt_age)
  
  plot_data <- plot_data %>% mutate(x_var = paste0(part_age_group, '_', part_gender),
                                    y_var = paste0(cnt_age_group, '_', cnt_gender))
  
  plot_data$part_age_group <- factor(plot_data$part_age_group, levels = rev(unique(plot_data$part_age_group)))
  plot_data$cnt_age_group <- factor(plot_data$cnt_age_group, levels = unique(plot_data$cnt_age_group))
  
  flattened_matrix <- ggplot(plot_data, 
         aes(x = part_gender, y = cnt_gender, fill = predicted_contacts)) + 
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "Mean contacts", 
                         limits = c(0, max(plot_data$predicted_contacts, na.rm = TRUE)),
                         trans = scales::pseudo_log_trans(sigma = 2), breaks = c(0,1,2,3,4,5,6)) +
    labs(
      title = sprintf("Mean Contacts: Age Group x Gender"),
      subtitle = paste("Country:", target_country, "| Model: GAM (Tensor Spline + Gender Interaction)"),
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
  ggsave(paste0(plot_filename_base, "_flattened_matrix.png"), plot = flattened_matrix, width = 12, height = 10, dpi = 600, bg="white")

  # Arrange plots using patchwork
  if (length(plots_list) == 4) {
      combined_plot <- (plots_list[["F_F"]] | plots_list[["F_M"]]) / (plots_list[["M_F"]] | plots_list[["M_M"]])
      plot_title <- paste("GAM-based Contact Matrices (Age x Gender) - ", target_country)
      combined_plot <- combined_plot + plot_annotation(title = plot_title, theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold")))

      # --- Save Outputs ---
      output_dir <- "output/gam_matrices"
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Save the combined plot
      plot_filename_base <- file.path(output_dir, paste0("gam_contact_matrix_", target_country, "_age_gender"))
      ggsave(paste0(plot_filename_base, ".png"), plot = combined_plot, width = 12, height = 10, dpi = 600, bg="white")
      ggsave(paste0(plot_filename_base, ".pdf"), plot = combined_plot, width = 12, height = 10, dpi = 600, bg="white")
      message(paste("\nCombined plot saved to:", paste0(plot_filename_base, ".png/.pdf")))

      # Save the results object (matrix and GAM fit)
      rds_filename <- file.path(output_dir, paste0("gam_results_", target_country, "_age_gender.rds"))
      saveRDS(gam_results, file = rds_filename)
      message(paste("GAM results object saved to:", rds_filename))

      # Display the plot if running interactively
      if (interactive()) {
          print(combined_plot)
      }
  } else {
      message("Expected 4 gender combination plots, but found ", length(plots_list), ". Combined plot not generated.")
  }

} else {
  message("\nGAM analysis failed. No results to process.")
}

message("\nDemo script finished.") 













  