#' Plot a multiverse analysis
#'
#' Combines density plot (top) and heatmap strips (bottom) into one visualization
#' of a multiverse analysis.
#'
#' @param use_case Character string specifying which dataset to use
#'   (currently "hurricane" or "beauty").
#' @param output_path Optional. File path to save the combined plot as PNG.
#'
#' @return A combined ggplot object (patchwork layout).
#'
#' @examples
#' plot_multiverse("hurricane")
#' \dontrun{
#'   plot_multiverse("hurricane", output_path = "plots/my_mva.png")
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr mutate %>% desc
#' @importFrom data.table :=
#' @importFrom stats as.formula coef density lm
#' @importFrom utils read.csv
plot_multiverse <- function(use_case = "hurricane", output_path = NULL) {
  prep <- prep_data(use_case = use_case)
  df <- prep[[1]]
  outcome_var <- prep[[2]]
  outcome_var_label <- prep[[3]]
  strip_vars <- prep[[4]]
  variable_labels <- prep[[5]]

  density_plot <- generate_density_plot(df, outcome_var, outcome_var_label)
  heatmap_strips <- generate_heatmap_strips(df, outcome_var, strip_vars, variable_labels)

  density_ratio <- 0.5
  n_strips <- length(strip_vars)
  heights_vector <- c(density_ratio * n_strips, rep(1, n_strips))

  combined_plot <- patchwork::wrap_plots(
    c(list(density_plot), heatmap_strips),
    ncol = 1,
    heights = heights_vector
  )

  if (!is.null(output_path)) {
    dir <- dirname(output_path)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    ggplot2::ggsave(output_path, combined_plot, width = 11.5, height = 6.12)
  }


  return(combined_plot)
}
