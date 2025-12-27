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


#' Line Chart Component
#'
#' Create a customizable line chart using ggplot2, with support for grouping, faceting, points, custom shapes, and last-point labels.
#'
#' @param data Data frame containing the variables.
#' @param x The x-axis variable.
#' @param y The y-axis variable.
#' @param group Optional grouping variable for line color and shape.
#' @param facet Optional faceting variable for creating multiple panels.
#' @param title Chart title.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param legend_title Title for the legend (applies to color and shape).
#' @param legend_position Position of the legend (e.g., "right", "top", "bottom", "left").
#' @param legend_direction Direction of the legend items ("vertical" or "horizontal").
#' @param note Caption or note displayed below the plot.
#' @param facet_scales Scales for facets ("fixed", "free", "free_x", "free_y").
#' @param facet_ncol Number of columns for facet layout.
#' @param show_points Logical, whether to display points on the lines.
#' @param point_shape Optional vector of integers specifying point shapes for each group.
#' @param point_size Size of the points.
#' @param line_size Size of the lines.
#' @param show_labels Logical, whether to show a label next to the last observation of each line.
#' @param label_size Size of the last-point labels.
#'
#' @returns A ggplot2 object representing the line chart.
#' @export
#'
line_chart_raw <- function(
    data,
    x,
    y,
    group = NULL,
    facet = NULL,
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_title = NULL,
    legend_position = "right",
    legend_direction = "vertical",
    note = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    show_points = FALSE,
    point_shape = NULL,       # vector of shapes per group
    point_size = 3.5,
    line_size = 1,
    show_labels = FALSE,      # label last point per group
    label_size = 3
) {

  # ---- Capture expressions
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  group_quo <- rlang::enquo(group)
  facet_quo <- rlang::enquo(facet)

  # ---- Validate required aesthetics
  if (rlang::quo_is_null(x_quo)) rlang::abort("`x` must be supplied.")
  if (rlang::quo_is_null(y_quo)) rlang::abort("`y` must be supplied.")

  df <- data

  # ---- Base aesthetics mapping
  aes_mapping <- ggplot2::aes(
    x = !!x_quo,
    y = !!y_quo,
    group = !!group_quo,
    color = !!group_quo
  )

  # ---- Build base plot with line
  p <- ggplot2::ggplot(df, aes_mapping) +
    ggplot2::geom_line(linewidth = line_size)

  # ---- Add points with optional custom shapes
  if (show_points) {
    if (!is.null(point_shape) && !rlang::quo_is_null(group_quo)) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(shape = !!group_quo), size = point_size) +
        ggplot2::scale_shape_manual(values = point_shape)
    } else {
      p <- p + ggplot2::geom_point(size = point_size)
    }
  }

  # ---- Add labels for last point per group (and per facet if applicable)
  if (show_labels && !rlang::quo_is_null(group_quo)) {
    if (!rlang::quo_is_null(facet_quo)) {
      last_points <- df %>%
        dplyr::group_by(!!group_quo, !!facet_quo) %>%
        dplyr::slice_max(!!x_quo, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    } else {
      last_points <- df %>%
        dplyr::group_by(!!group_quo) %>%
        dplyr::slice_max(!!x_quo, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    }

    p <- p + ggrepel::geom_text_repel(
      data = last_points,
      ggplot2::aes(label = !!group_quo),
      size = label_size,
      nudge_x = 0.2,
      show.legend = FALSE
    )
  }

  # ---- Facets
  if (!rlang::quo_is_null(facet_quo)) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet_quo),
        scales = facet_scales,
        ncol = facet_ncol
      )
  }

  # ---- Labels & theme
  p <- p + ggplot2::labs(
    title = title,
    x = x_label,
    y = y_label,
    color = legend_title,
    caption = note
  )

  # Only add shape legend title if shape mapping exists
  if (!is.null(point_shape) && !rlang::quo_is_null(group_quo)) {
    p <- p + ggplot2::labs(shape = legend_title)
  }

  # ---- Hide legend if no group
  if (!(".group" %in% names(df)) && rlang::quo_is_null(group_quo)) {
    p <- p + ggplot2::guides(color = "none", shape = "none")
  }

  p
}

#' Line Chart Component
#'
#' @param data Data Frame
#' @param x The x variable
#' @param y The y variable
#' @param group A grouping variable
#' @param facet A faceting variable
#' @param title Plot title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param legend_title Legend title
#' @param legend_position Legend position
#' @param legend_direction Legend direction
#' @param note Caption / Note
#' @param facet_scales Scales for facets
#' @param facet_ncol Number of columns for facets
#' @param show_points Show points on line
#' @param point_shape Optional vector of shapes per group
#' @param point_size Size of points
#' @param line_size Size of line
#' @param show_labels Show labels on last point per group
#' @param label_size Size of labels
#'
#' @returns A ggplot2 object
#' @export
line_chart <- function(
    data,
    x,
    y,
    group = NULL,
    facet = NULL,
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_title = NULL,
    legend_position = "right",
    legend_direction = "vertical",
    note = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    show_points = FALSE,
    point_shape = NULL,
    point_size = 3.5,
    line_size = 1,
    show_labels = FALSE,
    label_size = 3
) {

  # ---- Capture expressions
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  group_quo <- rlang::enquo(group)
  facet_quo <- rlang::enquo(facet)

  # ---- Validate required aesthetics
  if (rlang::quo_is_null(x_quo)) rlang::abort("`x` must be supplied.")
  if (rlang::quo_is_null(y_quo)) rlang::abort("`y` must be supplied.")

  # ---- Evaluate variables
  df <- eval_plot_vars(data, x_quo, y_quo, group_quo, facet_quo)

  # ---- Build base line plot
  p <- line_build_base_plot(df, x_quo, y_quo, group_quo, line_size)

  # ---- Add points
  if (show_points) {
    p <- line_add_points(p, df, group_quo, point_shape, point_size)
  }

  # ---- Add labels for last point per group
  if (show_labels && !rlang::quo_is_null(group_quo)) {
    p <- line_add_last_point_labels(p, df, x_quo, group_quo, facet_quo, label_size)
  }

  # ---- Add facets
  p <- add_facets(p, df, facet_scales, facet_ncol)

  # ---- Add theme and labels (merged legend)
  p <- add_common_theme(
    p,
    title,
    x_label,
    y_label,
    note,
    legend_position,
    legend_direction
  )

  if (!is.null(legend_title) && !rlang::quo_is_null(group_quo)) {

    # Always set color legend
    p <- p + ggplot2::labs(color = legend_title)

    # Only set shape legend if shape is actually mapped
    if (show_points && !is.null(point_shape)) {
      p <- p + ggplot2::labs(shape = legend_title)
    }
  }

  # ---- Hide legend if no group
  if (!(".group" %in% names(df)) && rlang::quo_is_null(group_quo)) {
    p <- p + ggplot2::guides(color = "none", shape = "none")
  }

  p
}

