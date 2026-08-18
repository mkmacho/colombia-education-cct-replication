#!/usr/bin/env Rscript

# Reproducible baseline replication for Barrera-Osorio et al. (2011).
# The design intentionally avoids automatic package installation and uses only
# the official data file documented in data/README.md.

required_packages <- c("haven", "sandwich", "lmtest")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install missing packages before running: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

data_path <- file.path(
  "data", "raw", "113783-V1", "AEJApp_2010-0132_Data",
  "Public_Data_AEJApp_2010-0132.dta"
)
if (!file.exists(data_path)) {
  stop("Missing ", data_path, ". See data/README.md for the canonical source package.", call. = FALSE)
}

data <- haven::read_dta(data_path)
required_columns <- c(
  "at_msamean", "T1_treat", "T2_treat", "T3_treat", "suba",
  "survey_selected", "grade", "school_code", "s_teneviv", "s_utilities",
  "s_durables", "s_infraest_hh", "s_age_sorteo", "s_age_sorteo2",
  "s_years_back", "s_sexo", "s_estcivil", "s_single", "s_edadhead",
  "s_yrshead", "s_tpersona", "s_num18", "s_estrato", "s_puntaje",
  "s_ingtotal", "s_over_age"
)
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns)) {
  stop("Input file does not match the archived schema; missing: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

estimate <- function(frame, formula, model_name) {
  fit <- stats::lm(formula, data = frame)
  vcov <- sandwich::vcovCL(fit, cluster = frame[["school_code"]], type = "HC1")
  result <- lmtest::coeftest(fit, vcov. = vcov)
  keep <- intersect(c("T1_treat", "T2_treat", "T3_treat"), rownames(result))
  data.frame(
    model = model_name,
    formula = paste(deparse(formula), collapse = " "),
    term = keep,
    estimate = result[keep, "Estimate"],
    std_error_clustered_school = result[keep, "Std. Error"],
    statistic = result[keep, "t value"],
    p_value = result[keep, "Pr(>|t|)"],
    n = stats::nobs(fit),
    n_schools = length(unique(frame[["school_code"]])),
    row.names = NULL,
    check.names = FALSE
  )
}

analysis <- subset(data, !(suba == 1 & grade < 9) & survey_selected == 1 & grade != 11)
basic <- subset(analysis, suba == 0)
tertiary <- subset(analysis, suba == 1 & grade > 8)
all_older_students <- subset(analysis, grade > 8)
if (!nrow(basic) || !nrow(tertiary) || !nrow(all_older_students)) {
  stop("The treatment subsamples are empty; verify the input version and variables.", call. = FALSE)
}

controls <- paste(
  c(
    "factor(s_teneviv)", "s_utilities", "s_durables", "s_infraest_hh",
    "s_age_sorteo", "s_age_sorteo2", "s_years_back", "s_sexo",
    "factor(s_estcivil)", "s_single", "s_edadhead", "s_yrshead",
    "s_tpersona", "s_num18", "factor(s_estrato)", "s_puntaje",
    "s_ingtotal", "factor(grade)", "suba", "s_over_age"
  ),
  collapse = " + "
)
school_fixed_effects <- "factor(school_code)"

results <- rbind(
  estimate(basic, at_msamean ~ T1_treat + T2_treat, "table_03_col_1"),
  estimate(basic, stats::as.formula(paste("at_msamean ~ T1_treat + T2_treat +", controls)), "table_03_col_2"),
  estimate(basic, stats::as.formula(paste("at_msamean ~ T1_treat + T2_treat +", controls, "+", school_fixed_effects)), "table_03_col_3"),
  estimate(tertiary, at_msamean ~ T3_treat, "table_03_col_4"),
  estimate(tertiary, stats::as.formula(paste("at_msamean ~ T3_treat +", controls)), "table_03_col_5"),
  estimate(tertiary, stats::as.formula(paste("at_msamean ~ T3_treat +", controls, "+", school_fixed_effects)), "table_03_col_6"),
  estimate(all_older_students, stats::as.formula(paste("at_msamean ~ T1_treat + T2_treat + T3_treat +", controls, "+", school_fixed_effects)), "table_03_col_7")
)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(results, file.path("outputs", "attendance_treatment_effects.csv"), row.names = FALSE)
metadata <- c(
  "Data: Public_Data_AEJApp_2010-0132.dta",
  paste("R:", R.version.string),
  paste("haven:", as.character(utils::packageVersion("haven"))),
  paste("sandwich:", as.character(utils::packageVersion("sandwich"))),
  paste("lmtest:", as.character(utils::packageVersion("lmtest")))
)
writeLines(metadata, file.path("outputs", "run_metadata.txt"))
message("Wrote outputs/attendance_treatment_effects.csv and outputs/run_metadata.txt")
