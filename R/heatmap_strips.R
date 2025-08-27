#' Generate heatmap strips for multiverse outcomes
#'
#' Creates a set of heatmap strip plots that visualize how outcome estimates
#' vary depending on analytical decision variables in a multiverse analysis.
#'
#' @param data A dataframe with results of the multiverse analysis.
#' @param outcome_var Character. Name of the outcome variable.
#' @param strip_vars Character vector. Names of decision variables to display.
#' @param variable_labels Named vector. Display labels for the decision variables.
#'
#' @return A list of ggplot objects representing heatmap strips.
#'
#' @examples
#' prep <- prep_data("hurricane")
#' df <- prep[[1]]
#' strip_vars <- prep[[4]]
#' variable_labels <- prep[[5]]
#' generate_heatmap_strips(df, "edif", strip_vars, variable_labels)
#'
#' @export
generate_heatmap_strips <- function(data, outcome_var, strip_vars, variable_labels) {
  n_bins <- 20
  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)
  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)
  breaks <- seq(x_min, x_max, length.out = n_bins + 1)

  # Fit linear model for reordering factor levels
  reg_formula <- as.formula(paste(outcome_var, "~", paste(strip_vars, collapse = " + ")))
  lm_model <- lm(reg_formula, data = data)
  coefs <- coef(lm_model)

  tidy_coefs <- tibble::enframe(coefs, name = "term", value = "estimate") %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      var = strip_vars[which.max(sapply(strip_vars, function(v) startsWith(term, v)))],
      level = trimws(sub(paste0("^", var), "", term))
    ) %>%
    dplyr::ungroup()

  # Reorder factor levels
  for (var in strip_vars) {
    ref_level <- levels(data[[var]])[1]
    coef_sub <- tidy_coefs %>% dplyr::filter(var == !!var & level != ref_level) %>%
      dplyr::arrange(desc(abs(estimate))) %>% dplyr::pull(level)
    data[[var]] <- factor(data[[var]], levels = unique(c(ref_level, coef_sub)))
  }

  # Label lookup for coefficients
  label_lookup <- tidy_coefs %>%
    dplyr::mutate(
      rounded = round(estimate, 2),
      label_text = dplyr::case_when(
        rounded > 0 ~ paste0("'+", formatC(rounded, format = "f", digits = 2), "'"),
        rounded < 0 ~ paste0("'", formatC(rounded, format = "f", digits = 2), "'"),
        TRUE ~ "'0.00'"
      )
    ) %>% dplyr::select(var, level, label_text)

  ref_labels <- lapply(strip_vars, function(var) {
    base_level <- levels(data[[var]])[1]
    tibble::tibble(var = var, level = base_level, label_text = "Ref.")
  }) %>% dplyr::bind_rows()

  label_lookup <- dplyr::bind_rows(label_lookup, ref_labels)

  # Order variables by influence
  influence_scores <- tidy_coefs %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(max_abs_effect = max(abs(estimate), na.rm = TRUE)) %>%
    dplyr::arrange(desc(max_abs_effect))

  strip_vars <- influence_scores$var
  base_colors <- viridis::viridis(length(strip_vars), option = "D")

  # Generate strips
  heatmap_strips <- Map(function(varname, base_color) {
    var_label <- variable_labels[[varname]]
    generate_heatmap_strip(data, varname, outcome_var, breaks, base_color, var_label, label_lookup, common_xlim)
  }, varname = strip_vars, base_color = base_colors)

  return(heatmap_strips)
}

#' @keywords internal
generate_heatmap_strip <- function(df, varname, outcome_var, breaks, base_color, var_label, label_lookup, common_xlim) {
  var_sym <- rlang::sym(varname)
  out_sym <- rlang::sym(outcome_var)
  x_min <- min(df[[outcome_var]], na.rm = TRUE)
  x_max <- max(df[[outcome_var]], na.rm = TRUE)
  x_label_pos <- x_max + 0.01 * (x_max - x_min)

  heatmap_data <- df %>%
    dplyr::mutate(outcome_bin = cut(!!out_sym, breaks = breaks, include.lowest = TRUE)) %>%
    dplyr::count(outcome_bin, !!var_sym) %>%
    dplyr::group_by(outcome_bin) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(outcome_bin, !!var_sym, fill = list(prop = 0)) %>%
    dplyr::mutate(
      bin_index = as.integer(outcome_bin),
      xmin = breaks[bin_index],
      xmax = breaks[bin_index + 1],
      !!var_sym := factor(!!var_sym, levels = levels(df[[varname]]))
    )

  heatmap_data <- add_variable_label_row(heatmap_data, varname, base_color, var_label)

  label_data <- heatmap_data %>%
    dplyr::filter(!is.na(!!var_sym), !is.na(prop)) %>%
    dplyr::distinct(!!var_sym) %>%
    dplyr::mutate(var = varname) %>%
    dplyr::rename(level_name = !!var_sym) %>%
    dplyr::left_join(label_lookup, by = c("var", "level_name" = "level")) %>%
    dplyr::mutate(
      label_text = ifelse(label_text == "Ref.", "italic('Ref.')", label_text),
      !!var_sym := factor(level_name, levels = levels(heatmap_data[[varname]]))
    )

  ref_level <- levels(df[[varname]])[1]

  ggplot2::ggplot(heatmap_data, ggplot2::aes(y = !!var_sym, fill = prop)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = as.numeric(!!var_sym) - 0.5,
                                    ymax = as.numeric(!!var_sym) + 0.5), color = NA) +
    ggplot2::geom_text(data = label_data,
                       ggplot2::aes(y = !!var_sym, label = label_text),
                       x = x_label_pos, inherit.aes = FALSE, hjust = 0, size = 3, parse = TRUE, na.rm = TRUE) +
    ggplot2::scale_fill_gradient(low = "white", high = base_color, na.value = NA) +
    ggplot2::scale_x_continuous(limits = common_xlim, expand = c(0, 0)) +
    ggplot2::scale_y_discrete(
      labels = function(labs) {
        ifelse(
          labs == "label", paste0("**", var_label, "**"),
          ifelse(labs == ref_level, paste0("*", labs, "*"), labs)
        )
      },
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(xlim = common_xlim) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 20, 0, 10)
    )
}

#' @keywords internal
add_variable_label_row <- function(data, varname, base_color, var_label) {
  orig_levels <- levels(data[[varname]])
  new_levels <- rev(c("label", orig_levels))
  data[[varname]] <- factor(data[[varname]], levels = new_levels)
  label_row <- data.frame(
    outcome_bin = NA,
    prop = NA,
    xmin = min(data$xmin, na.rm = TRUE),
    xmax = max(data$xmax, na.rm = TRUE)
  )
  label_row[[varname]] <- factor("label", levels = new_levels)
  for (col in setdiff(names(data), names(label_row))) label_row[[col]] <- NA
  dplyr::bind_rows(label_row[, names(data)], data)
}
