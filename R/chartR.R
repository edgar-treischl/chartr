#' Bar Chart Component
#'
#' @param data Data Frame
#' @param x The x variable
#' @param y The y variable
#' @param group A grouping variable
#' @param facet A faceting variable
#' @param stacked Stacked or dodged bars
#' @param horizontal Horizontal bars
#' @param title Title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param legend_title Legend title
#' @param legend_position Legend position
#' @param legend_direction Legend direction
#' @param note Caption / Note
#' @param facet_scales Scales for facets
#' @param facet_ncol Number of columns for facets
#' @param show_labels Show labels on bars
#' @param label_size Size of labels
#'
#' @returns A ggplot2 object
#' @export
#'
bar_chart <- function(
    data,
    x,
    y,
    group = NULL,
    facet = NULL,
    stacked = FALSE,
    horizontal = FALSE,
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_title = NULL,
    legend_position = "right",
    legend_direction = "vertical",
    note = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    show_labels = FALSE,
    label_size = 3.5
) {

  # ---- Capture expressions ONCE
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  group_quo <- rlang::enquo(group)
  facet_quo <- rlang::enquo(facet)

  # ---- Validate required aesthetics
  if (rlang::quo_is_null(x_quo)) {
    rlang::abort("`x` must be supplied.")
  }
  if (rlang::quo_is_null(y_quo)) {
    rlang::abort("`y` must be supplied.")
  }

  # ---- Evaluate variables
  df <- eval_plot_vars(
    data = data,
    x = x_quo,
    y = y_quo,
    group = group_quo,
    facet = facet_quo
  )

  # ---- Summarise
  data_summary <- bar_summarize_data(df)

  # ---- Bar position
  position <- if (stacked) {
    ggplot2::position_stack()
  } else {
    ggplot2::position_dodge(width = if (horizontal) 0.8 else 0.9)
  }

  # ---- Build plot
  p <- bar_build_base_plot(data_summary, legend_title) +
    ggplot2::geom_col(position = position)

  # ---- Labels
  if (show_labels) {
    p <- bar_add_labels(p, data_summary, stacked, label_size)
  }

  # ---- Facets
  p <- add_facets(p, data_summary, facet_scales, facet_ncol)

  # ---- Theme & labels
  p <- add_common_theme(
    p,
    title,
    x_label,
    y_label,
    note,
    legend_position,
    legend_direction
  )

  # ---- Hide legend if no group
  if (!(".group" %in% names(data_summary))) {
    p <- p + ggplot2::guides(fill = "none")
  }

  # ---- Orientation
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }

  p
}
