#' Prepare multiverse analysis data
#'
#' Loads and prepares datasets for different use cases ("hurricane" or "beauty").
#'
#' @param use_case Character. Either "hurricane" or "beauty".
#'
#' @return A list containing:
#' \itemize{
#'   \item df: The prepared dataframe.
#'   \item outcome_var: The outcome variable name.
#'   \item outcome_var_label: The display label for the outcome variable.
#'   \item strip_vars: The decision variable names.
#'   \item variable_labels: The decision variable labels.
#' }
#'
#' @examples
#' prep <- prep_data("hurricane")
#' str(prep)
#'
#' @export
#' @import dplyr
#' @import magrittr
#' @importFrom utils read.csv
prep_data <- function(use_case) {
  if (use_case == "hurricane") {
    # --- Load and Prepare Data ---
    file_path <- system.file("extdata", "hurricane_mva_results.csv", package = "mvaplots")
    df <- read.csv(file_path, sep = ";")
    df <- df %>% mutate(significant = p < 0.05) # create binary variable indicating significant effect size

    # Define outcome and predictors
    outcome_var <- "edif" # represents the additional deaths when a hurricane has a feminine rather than a male sounding name

    # Name the outcome variable to be displayed in the plots
    outcome_var_label <- "Excess Fatalities"

    # Decision variables names
    strip_vars <- c("k1", "k2", "k3", "k4", "k5", "k6", "k7")

    # Variable display labels
    # These labels sum up the different possible decisions in a group k*
    # They will be the headings of the heatmap strips
    variable_labels <- c(
      k1 = "Outliers",
      k2 = "Leverage points",
      k3 = "Femininity",
      k4 = "Model",
      k5 = "Functional for damages",
      k6 = "Femininity: Main vs. interaction",
      k7 = "Controlling for year"
    )

    # Each group of decisions k* encompasses several options which we relabel,
    # so they have more meaningful labels
    df <- df %>% mutate(
      k1 = factor(k1, levels = c(3, 1, 2), labels = c("Drop 2 highest deaths", "Drop none", "Drop 1 highest deaths")),
      k2 = factor(k2, levels = c(1, 2, 3, 4), labels = c("Drop none", "Drop 1 highest damage", "Drop 2 highest damage", "Drop 3 highest damage")),
      k3 = factor(k3, levels = c(2, 1), labels = c("Femininity (1-11)", "Female (1/0)")),
      k4 = factor(k4, levels = c(2, 1), labels = c("Negative Binomial", "Log(fatalities+1)")),
      k5 = factor(k5, levels = c(1, 2), labels = c("Linear: $", "Log: ln($)")),
      k6 = factor(k6, levels = c(2, 1, 3, 4, 5, 6),
                  labels = c("X Damages & Min Pressure", "X Damages", "X Damages & Wind",
                             "X Damages & Hurricane Cat.", "X Damages & Pressure, Wind, Cat.", "Main effect")),
      k7 = factor(k7, levels = c(1, 2, 3), labels = c("None", "Year*Damages", "Post 1979 (1/0)*Damages"))
    )
    return(list(df, outcome_var, outcome_var_label, strip_vars, variable_labels))
  }
  else if (use_case == "beauty") {
    # --- Load and Prepare Data ---
    file_path <- system.file("extdata", "beauty_mva_results.csv", package = "mvaplots")
    df <- read.csv(file_path, sep = ";")

    df <- df %>% mutate(significant = p_b < 0.05)

    # Define outcome
    outcome_var <- "b"

    # Name the outcome variable to be displayed in the plots
    outcome_var_label <- "Beauty premium"

    # Decision variables names
    strip_vars <- c("k1", "k2", "k3", "k4", "k5", "k6")

    # Variable display labels
    # These labels sum up the different possible decisions in a group k*
    # They will be the headings of the heatmap strips
    variable_labels <- c(
      k1 = "Age restriction",
      k2 = "Interviewer exclusions",
      k3 = "Controls",
      k4 = "Treatment",
      k5 = "Outcome",
      k6 = "Model"
    )

    # Each group of decisions k* encompasses several options which we relabel,
    # so they have more meaningful labels
    df <- df %>% mutate(
      k1 = factor(k1,
                  levels = c(1, 2),
                  labels = c("Age > 18", "All respondents")),

      k2 = factor(k2,
                  levels = c(1, 2, 3),
                  labels = c("Excluded if no variation",
                             "Excluded if little variation",
                             "All interviewers")),

      k3 = factor(k3,
                  levels = c(1, 2, 3, 4),
                  labels = c("All", "Personality", "Occupation", "Education")),

      k4 = factor(k4,
                  levels = c(1, 2),
                  labels = c("Binary", "Continuous")),

      k5 = factor(k5,
                  levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("ln gross, trimmed",
                             "ln gross, winzorized",
                             "ln gross",
                             "ln net, trimmed",
                             "ln net, winzorized",
                             "ln net")),

      k6 = factor(k6,
                  levels = c(1, 2, 3),
                  labels = c("Interviewer Fixed Effects",
                             "Respondents Random Effects",
                             "Interviewer Random Effects"))
    )
  return(list(df, outcome_var, outcome_var_label, strip_vars, variable_labels))
  }
}
