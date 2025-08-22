#' Generate density plot of multiverse analysis results
#'
#' This function creates a density plot comparing the distribution of all
#' estimated outcomes with the distribution of statistically significant outcomes
#' in a multiverse analysis.
#'
#' @param data A dataframe containing results of the multiverse analysis.
#'   Must include the outcome variable and a binary column `significant`.
#' @param outcome_var Character. The name of the outcome variable to plot.
#' @param outcome_var_label Character. Label for the outcome variable (x-axis).
#'
#' @return A ggplot object representing the density plot.
#'
#' @examples
#' prep <- prep_data("hurricane")
#' df <- prep[[1]]
#' generate_density_plot(df, "edif", "Excess Fatalities")
#'
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate desc
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom stats as.formula coef density lm
#' @importFrom utils read.csv
generate_density_plot <- function(data, outcome_var, outcome_var_label) {
  # Find range of outcome variable
  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)

  # Density for all values
  dens_all <- density(data[[outcome_var]], from = x_min, to = x_max)

  # Density for significant values
  dens_sig <- density(data[[outcome_var]][data$significant], from = x_min, to = x_max)
  sig_share <- mean(data$significant, na.rm = TRUE)
  dens_sig$y <- pmin(dens_sig$y * sig_share, dens_all$y)

  # Transform to data frames
  dens_all_df <- data.frame(x = dens_all$x, y = dens_all$y, type = "All")
  dens_sig_df <- data.frame(x = dens_sig$x, y = dens_sig$y, type = "Significant")
  df_dens <- rbind(dens_all_df, dens_sig_df)
  df_dens$type <- factor(df_dens$type, levels = c("All", "Significant"))

  # Set common x-axis
  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)

  # Create density plot
  density_plot <- ggplot2::ggplot(df_dens, ggplot2::aes(x = x, y = y, color = type, fill = type)) +
    ggplot2::geom_line(data = subset(df_dens, type == "All"), size = 1) +
    ggplot2::geom_area(data = subset(df_dens, type == "Significant"), alpha = 0.3, color = NA) +
    ggplot2::scale_color_manual(values = c("steelblue", "tomato")) +
    ggplot2::scale_fill_manual(values = c("steelblue", "tomato")) +
    ggplot2::labs(
      x = outcome_var_label,
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::coord_cartesian(xlim = common_xlim) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.key.height = grid::unit(0.3, "cm"),
      legend.text = ggplot2::element_text(size = 9),
      legend.position.inside = c(0.9, 0.95),
      legend.justification = c("right", "top"),
      legend.direction = "vertical",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  return(density_plot)
}
