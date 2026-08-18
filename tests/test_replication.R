#!/usr/bin/env Rscript

# This test is intentionally skipped in a clean clone: source data must be
# downloaded manually under the license described in data/README.md.
data_path <- file.path(
  "data", "raw", "113783-V1", "AEJApp_2010-0132_Data",
  "Public_Data_AEJApp_2010-0132.dta"
)
if (!file.exists(data_path)) {
  message("Skipping data-dependent replication check: official input not present.")
  quit(status = 0)
}

status <- system2("Rscript", "scripts/replicate_tables.R")
if (status != 0) stop("Replication script failed.", call. = FALSE)

results_path <- file.path("outputs", "attendance_treatment_effects.csv")
if (!file.exists(results_path)) stop("Replication script did not write results.", call. = FALSE)
results <- utils::read.csv(results_path, check.names = FALSE)
if (nrow(results) != 12L) stop("Expected 12 treatment-effect estimates.", call. = FALSE)

first_effect <- subset(results, model == "table_03_col_1" & term == "T1_treat")
if (nrow(first_effect) != 1L) stop("Missing Table 3 column 1 T1 estimate.", call. = FALSE)
if (abs(first_effect$estimate - 0.0325429036981351) > 1e-10) {
  stop("Table 3 column 1 T1 estimate differs from the validated baseline result.", call. = FALSE)
}
if (abs(first_effect$std_error_clustered_school - 0.0074988488407975) > 1e-10) {
  stop("Table 3 column 1 T1 clustered standard error differs from the validated baseline result.", call. = FALSE)
}

message("Passed data-dependent regression checks.")
