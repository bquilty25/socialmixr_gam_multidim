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

# Define a simpler formula without age*gender smooth interactions
# Note: k values should match those defined below for consistency in comparison
gam_formula_simple <- N ~ te(part_age, cnt_age, k=c(8,8), bs="ps") +
                        interaction(part_gender, cnt_gender)

# Define k settings and family once for consistency
k_tensor_val <- c(8, 8)
k_by_val <- 6
bs_numeric_val <- "ps"
gam_family <- nb() # Store family used

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

# --- Run gam_contact_matrix with Weighting ---
message("\nRunning gam_contact_matrix function with weighting enabled...")

# Ensure wppExplorer or a similar package providing wpp_age is available if using weigh.age with country names
# install.packages("wppExplorer") # Example if needed
# library(wppExplorer) # Contains wpp_age

# Get UK population data for age weighting (example using wpp_age)
# You might need to install wppExplorer or provide a data frame directly
uk_pop <- NULL
if (requireNamespace("wppExplorer", quietly = TRUE)) {
  # Try loading pop data. Handling potential errors if data format changed.
  tryCatch({ 
    data(pop, package = "wppExplorer") # Load population data from wppExplorer
    if (exists("pop") && "country" %in% names(pop)) {
        uk_pop_raw <- as.data.table(pop)[country == "United Kingdom"]
        # Aggregate to lower.age.limit format if needed (depends on wppExplorer version)
        # Example assumes 'age' column exists for single years
        if ("age" %in% names(uk_pop_raw)) {
            uk_pop_raw[, lower.age.limit := age]
            # Select latest year available
            latest_year <- max(uk_pop_raw$year)
            uk_pop <- uk_pop_raw[year == latest_year, .(population = sum(population)), by = lower.age.limit]
            message("Using UK population data from wppExplorer for year ", latest_year)
        } else if ("lower.age.limit" %in% names(uk_pop_raw)) {
             # If already aggregated by age groups, use that
             latest_year <- max(uk_pop_raw$year)
             uk_pop <- uk_pop_raw[year == latest_year, .(population = sum(population)), by = lower.age.limit]
             message("Using UK population data (aggregated) from wppExplorer for year ", latest_year)
        } else {
            warning("Could not find single-year 'age' or 'lower.age.limit' column in wppExplorer pop data. Cannot use for age weighting.")
        }
    } else {
         warning("wppExplorer loaded, but 'pop' data object not found or missing 'country' column.")
    }
  }, error = function(e) {
      warning("Error loading or processing population data from wppExplorer: ", e$message)
  })
} else {
  warning("Package wppExplorer not found. Cannot perform age weighting by country name.")
  # Provide a dummy data frame if you have population data elsewhere:
  # uk_pop <- data.frame(lower.age.limit = seq(0, 100, 1), population = runif(101, 1e5, 1e6))
}


# Ensure the gam_contact_matrix function is loaded (make sure updated version is used)
source("R/contact_matrix.R") 

gam_results_weighted <- tryCatch({
  gam_contact_matrix(
    survey = polymod,
    countries = target_country,
    dimensions = dimensions,
    dim_breaks = dimension_breaks,
    family = gam_family,
    age_limits = analysis_age_limits,
    # --- Weighting arguments ---
    weigh.dayofweek = TRUE,
    weigh.age = !is.null(uk_pop), # Only weigh age if pop data is available
    survey.pop = uk_pop,
    # user_weights = NULL, # Add column names if you have custom weights
    weight.threshold = 5, # Example threshold
    # --- Optional controls for smooths ---
    k_tensor = k_tensor_val,
    k_by = k_by_val,
    bs_numeric = bs_numeric_val
  )
}, error = function(e) {
  message("\nError running weighted gam_contact_matrix: ", e$message)
  print(e) # Print more details on error
  return(NULL)
})

# --- Process and Visualise Weighted Results ---
if (!is.null(gam_results_weighted)) {
  message("\nWeighted GAM analysis complete. Processing results...")

  # Extract the predicted matrix (multidimensional array - represents predicted sum of weights)
  predicted_matrix_weighted <- gam_results_weighted$matrix
  cat("\nDimensions of the weighted predicted contact matrix:\n")
  print(dim(predicted_matrix_weighted))
  print(dimnames(predicted_matrix_weighted))

  # Example: Print the predicted value for 20-24 yr old Females with 30-34 yr old Males
  if (all(c("part_age", "cnt_age", "part_gender", "cnt_gender") %in% names(dimnames(predicted_matrix_weighted)))) {
      pred_val_w <- predicted_matrix_weighted["[20,25)", "[30,35)", "F", "M"]
      cat(sprintf("\nPredicted weighted contacts (F [20,25) -> M [30,35)): %.3f\n", pred_val_w))
  }

  # --- Visualisation using ggplot2 ---
  message("\nCreating visualisations for weighted results...")

  # Convert the prediction grid and predictions into a data.table for plotting
  plot_data_weighted <- gam_results_weighted$prediction_grid
  plot_data_weighted$predicted_weighted_contacts <- as.vector(predicted_matrix_weighted)
  setDT(plot_data_weighted)

  # Generate age labels from breaks for plotting
  age_labels_part_w <- dimnames(predicted_matrix_weighted)$part_age # Use dimnames directly
  age_labels_cnt_w <- dimnames(predicted_matrix_weighted)$cnt_age   # Use dimnames directly

  # Factorise for correct ordering in ggplot
  # Use the actual age midpoints from prediction_grid for mapping to factor levels
  # Need to handle potential NAs if prediction grid doesn't align perfectly
  plot_data_weighted[, part_age_group := factor(
      age_labels_part_w[findInterval(part_age, dimension_breaks$part_age)], # Map midpoint to interval label
      levels = age_labels_part_w)]
  plot_data_weighted[, cnt_age_group := factor(
       age_labels_cnt_w[findInterval(cnt_age, dimension_breaks$cnt_age)], # Map midpoint to interval label
       levels = age_labels_cnt_w)]

  # Create a list to store plots
  plots_list_weighted <- list()

  # Loop through each gender combination for visualisation
  gender_pairs_w <- expand.grid(part_g = dimension_breaks$part_gender, cnt_g = dimension_breaks$cnt_gender)

  for (i in 1:nrow(gender_pairs_w)) {
    p_gender_w <- as.character(gender_pairs_w$part_g[i])
    c_gender_w <- as.character(gender_pairs_w$cnt_g[i])

    plot_subset_w <- plot_data_weighted[part_gender == p_gender_w & cnt_gender == c_gender_w]

    # Check if subset is empty before plotting
     if (nrow(plot_subset_w) == 0) {
        warning(paste("No weighted data for gender combination:", p_gender_w, "->", c_gender_w))
        next # Skip to next iteration
     }


    p_w <- ggplot(plot_subset_w, aes(x = part_age_group, y = cnt_age_group, fill = predicted_weighted_contacts)) +
      geom_tile(colour = "white", linewidth = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Predicted\nWeighted\nContacts", limits = c(0, max(plot_data_weighted$predicted_weighted_contacts, na.rm = TRUE))) +
      labs(
        title = sprintf("Predicted Weighted Contacts: %s Participants vs %s Contacts", p_gender_w, c_gender_w),
        subtitle = paste("Country:", target_country, "| Model: Weighted GAM (Tensor + Interactions)"),
        x = "Participant Age Group",
        y = "Contact Age Group"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=7),
        axis.text.y = element_text(size=7),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9),
        # Ensure axis labels are displayed even if some factors levels have no data in this subset
         axis.text = element_text(colour = "black"),
         axis.ticks = element_line(colour = "grey80"),
         panel.grid.major = element_blank(), # Optional: remove grid lines for clarity
         panel.grid.minor = element_blank()
      ) +
       # Ensure all levels are shown on axes
       scale_x_discrete(drop = FALSE) +
       scale_y_discrete(drop = FALSE)


    plots_list_weighted[[paste(p_gender_w, c_gender_w, sep = "_")]] <- p_w

  }

  # Arrange plots using patchwork
  if (length(plots_list_weighted) == 4) {
      combined_plot_weighted <- (plots_list_weighted[["F_F"]] | plots_list_weighted[["F_M"]]) / (plots_list_weighted[["M_F"]] | plots_list_weighted[["M_M"]])
      plot_title_weighted <- paste("Weighted GAM-based Contact Matrices - ", target_country)
      combined_plot_weighted <- combined_plot_weighted + plot_annotation(title = plot_title_weighted, theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold")))

      # --- Save Outputs ---
      output_dir_w <- "output/gam_matrices_weighted" # New directory for weighted results
      dir.create(output_dir_w, recursive = TRUE, showWarnings = FALSE)

      # Save the combined plot
      plot_filename_base_w <- file.path(output_dir_w, paste0("gam_contact_matrix_", target_country, "_weighted"))
      ggsave(paste0(plot_filename_base_w, ".png"), plot = combined_plot_weighted, width = 12, height = 10, dpi = 600, bg="white")
      ggsave(paste0(plot_filename_base_w, ".pdf"), plot = combined_plot_weighted, width = 12, height = 10, dpi = 600, bg="white")
      message(paste("\nCombined weighted plot saved to:", paste0(plot_filename_base_w, ".png/.pdf")))

      # Save the results object (matrix and GAM fit)
      rds_filename_w <- file.path(output_dir_w, paste0("gam_results_", target_country, "_weighted.rds"))
      saveRDS(gam_results_weighted, file = rds_filename_w)
      message(paste("Weighted GAM results object saved to:", rds_filename_w))

      # Display the plot if running interactively
      if (interactive()) {
          print(combined_plot_weighted)
      }
  } else {
      message("Expected 4 gender combination plots for weighted results, but found ", length(plots_list_weighted), ". Combined weighted plot not generated.")
      # Print individual plots if available for debugging
      # for(pname in names(plots_list_weighted)) print(plots_list_weighted[[pname]])
  }

} else {
  message("\nWeighted GAM analysis failed or was skipped. No results to process.")
}


# --- Optional: Compare Weighted vs Unweighted AIC ---
# Load unweighted results if needed (assuming the original demo script was run and saved)
unweighted_results_path <- file.path("output/gam_matrices", paste0("gam_results_", target_country, "_age_gender.rds"))
gam_results_unweighted <- NULL
if (file.exists(unweighted_results_path)) {
    gam_results_unweighted <- readRDS(unweighted_results_path)
}

if (!is.null(gam_results_weighted) && !is.null(gam_results_unweighted)) {
    message("\nComparing Weighted vs Unweighted GAM Models (AIC)...")

    # Compare AIC
    aic_weighted <- AIC(gam_results_weighted$gam_fit)
    aic_unweighted <- AIC(gam_results_unweighted$gam_fit)

    cat("\n--- AIC Comparison ---\n")
    cat(sprintf("AIC (Weighted Model):   %.2f\n", aic_weighted))
    cat(sprintf("AIC (Unweighted Model): %.2f\n", aic_unweighted))

    if (aic_weighted < aic_unweighted) {
        cat("The weighted model provides a better fit based on AIC.\n")
    } else if (aic_unweighted < aic_weighted) {
        cat("The unweighted model provides a better fit based on AIC.\n")
    } else {
        cat("Both models have similar AIC values.\n")
    }

     # Note: Direct ANOVA comparison might be complex if the response variable differs (N vs sum_weights)
     # and weights are used differently. AIC is a more straightforward comparison here.

} else {
    message("\nSkipping AIC comparison (weighted or unweighted results missing).")
}

# --- Compare Models (Workaround 3: Direct Fitting for BOTH models) ---
# Re-aggregate data and fit BOTH complex and simple models directly in this script's environment
# to bypass potential issues returning the gam object from the function.

message("\nComparing GAM models (using direct fitting workaround for both models)...")

# 1a. Define the complex formula string manually (based on default function logic)
complex_formula_str <- paste(
  paste0("N ~ te(part_age, cnt_age, k = c(", k_tensor_val[1], ", ", k_tensor_val[2], "), bs = \"", bs_numeric_val, "\")"),
  "interaction(part_gender, cnt_gender)",
  paste0("s(part_age, by = interaction(part_gender, cnt_gender), k = ", k_by_val, ", bs = \"", bs_numeric_val, "\")"),
  paste0("s(cnt_age, by = interaction(part_gender, cnt_gender), k = ", k_by_val, ", bs = \"", bs_numeric_val, "\")"),
  sep = " + "
)
complex_formula_direct <- as.formula(complex_formula_str)
message("Manually defined complex formula for direct fitting: ", complex_formula_str)

# 1b. Define the simple formula string manually
# (Matches gam_formula_simple defined earlier, but creating string here for clarity)
simple_formula_str <- paste(
    paste0("N ~ te(part_age, cnt_age, k=c(", k_tensor_val[1], ",", k_tensor_val[2], "), bs=\"", bs_numeric_val, "\")"),
    "interaction(part_gender, cnt_gender)",
    sep = " + "
)
simple_formula_direct <- as.formula(simple_formula_str)
message("Manually defined simple formula for direct fitting: ", simple_formula_str)

# 2. Prepare and aggregate data ONCE
if (!is.null(gam_results)) { # Check the first function call didn't fail completely
    message("Preparing data for direct fitting...")
    survey_data_prep <- copy(polymod)
    # Filter by country
    if (!is.null(target_country) && "country" %in% names(survey_data_prep$participants)) {
        survey_data_prep$participants <- survey_data_prep$participants[country %in% target_country]
        survey_data_prep$contacts <- survey_data_prep$contacts[part_id %in% survey_data_prep$participants$part_id]
    }
    # Ensure age columns exist (simplified)
    if (!"part_age" %in% names(survey_data_prep$participants)) survey_data_prep$participants[, part_age := as.integer(part_age_exact)]
    if (!"cnt_age" %in% names(survey_data_prep$contacts)) survey_data_prep$contacts[, cnt_age := as.integer(cnt_age_exact)]
    # Apply age limits
    if (!is.null(analysis_age_limits)) {
        min_age <- analysis_age_limits[1]
        max_age <- analysis_age_limits[2]
        survey_data_prep$participants <- survey_data_prep$participants[part_age >= min_age & part_age <= max_age]
        survey_data_prep$contacts <- survey_data_prep$contacts[cnt_age >= min_age & cnt_age <= max_age]
        part_ids_remain <- unique(survey_data_prep$participants$part_id)
        survey_data_prep$contacts <- survey_data_prep$contacts[part_id %in% part_ids_remain]
        contact_ids_remain <- unique(survey_data_prep$contacts$part_id)
        survey_data_prep$participants <- survey_data_prep$participants[part_id %in% contact_ids_remain]
    }
    # Merge
    gam_data_full_direct <- merge(
        survey_data_prep$contacts,
        survey_data_prep$participants[, .(part_id, part_age, part_gender)],
        by = "part_id"
    )
    # Convert factors
    gam_data_full_direct[, part_gender := as.factor(part_gender)]
    gam_data_full_direct[, cnt_gender := as.factor(cnt_gender)]
    # Aggregate
    gam_data_agg_direct <- gam_data_full_direct[, .N, by = c("part_age", "cnt_age", "part_gender", "cnt_gender")]
    message("Data preparation and aggregation complete.")

    # 3a. Fit complex model directly
    message("Fitting complex model directly...")
    complex_gam_fit_direct <- tryCatch({
        mgcv::gam(
            formula = complex_formula_direct,
            data = gam_data_agg_direct,
            family = gam_family, # Use stored family
            method = "REML"
        )
    }, error = function(e) {
        message("Error fitting complex model directly: ", e$message)
        return(NULL)
    })

    # 3b. Fit simple model directly
    message("Fitting simple model directly...")
    simple_gam_fit_direct <- tryCatch({
        mgcv::gam(
            formula = simple_formula_direct,
            data = gam_data_agg_direct,
            family = gam_family, # Use stored family
            method = "REML"
        )
    }, error = function(e) {
        message("Error fitting simple model directly: ", e$message)
        return(NULL)
    })


    # 4. Compare the two directly fitted models
    if (is.null(complex_gam_fit_direct) || is.null(simple_gam_fit_direct)) {
        message("Direct fitting of one or both models failed. Cannot compare.")
    } else {
        message("Direct fitting of both models successful. Proceeding with comparison...")
        model_comparison <- tryCatch({
            anova(simple_gam_fit_direct, complex_gam_fit_direct, test = "Chisq") # Note order: simple, complex
        }, error = function(e) {
            message("Error during anova() comparison: ", e$message)
            return(NULL)
        })

        if (!is.null(model_comparison)) {
            cat("\n--- Model Comparison (ANOVA) ---\n")
            print(model_comparison)
        } else {
            message("ANOVA comparison failed.")
        }

        # Compare AIC
        aic_complex <- AIC(complex_gam_fit_direct)
        aic_simple <- AIC(simple_gam_fit_direct)
        cat("\n--- AIC Comparison ---\n")
        cat(sprintf("AIC (Complex Model with Interaction): %.2f\n", aic_complex))
        cat(sprintf("AIC (Simple Model no Interaction):    %.2f\n", aic_simple))
        if (aic_complex < aic_simple) {
            cat("The complex model with age-gender interactions provides a better fit based on AIC.\n")
        } else if (aic_simple < aic_complex) {
            cat("The simpler model without age-gender smooth interactions provides a better fit based on AIC.\n")
        } else {
            cat("Both models have similar AIC values.\n")
        }
    }
} else {
    message("\nInitial complex model function call failed (gam_results is NULL), skipping direct fitting and comparison.")
}

message("\nDemo script finished.") 