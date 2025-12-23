#' Line Chart Component
#'
#' @param data Data frame to be used for the chart
#' @param x X axis variable
#' @param y Y axis variable
#' @param group Grouping variable for multiple lines (optional)
#' @param title Chart title (optional)
#' @param x_label X axis label (optional)
#' @param y_label Y axis label (optional)
#'
#' @returns A ggplot2 line chart object
#' @export
#'
line_chart <- function(
    data,
    x,
    y,
    group = NULL,
    title = NULL,
    x_label = NULL,
    y_label = NULL
) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = {{ x }},
      y = {{ y }},
      color = {{ group }}
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    scale_color_mc() +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    )+
    theme_mc()

  # Hide legend if group is NULL
  if (is.null(group)) {
    p <- p + ggplot2::guides(color = "none")
  }

  return(p)
}





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

  # Capture expressions
  x_expr <- rlang::enquo(x)
  y_expr <- rlang::enquo(y)
  group_expr <- rlang::enquo(group)
  facet_expr <- rlang::enquo(facet)

  df <- data

  # Evaluate x and y inside data
  df <- df |>
    dplyr::mutate(
      .x = rlang::eval_tidy(x_expr, df),
      .y = rlang::eval_tidy(y_expr, df)
    )

  # Evaluate group and facet if provided
  if (!rlang::quo_is_null(group_expr)) {
    df <- df |>
      dplyr::mutate(.group = rlang::eval_tidy(group_expr, df))
  }
  if (!rlang::quo_is_null(facet_expr)) {
    df <- df |>
      dplyr::mutate(.facet = rlang::eval_tidy(facet_expr, df))
  }

  # Determine grouping vars for summarization
  group_vars <- list(quote(.x))
  if (".group" %in% colnames(df)) group_vars <- c(group_vars, quote(.group))
  if (".facet" %in% colnames(df)) group_vars <- c(group_vars, quote(.facet))

  # Summarize
  data_summary <- df |>
    dplyr::group_by(!!!group_vars) |>
    dplyr::summarise(.y = mean(.y), .groups = "drop")

  # Labels data
  if (show_labels) data_labels <- data_summary

  # Bar position
  bar_position <- if (stacked) ggplot2::position_stack() else ggplot2::position_dodge(width = if (horizontal) 0.8 else 0.9)

  # Base plot
  p <- ggplot2::ggplot(data_summary, ggplot2::aes(
    x = .x,
    y = .y,
    fill = if (".group" %in% colnames(data_summary)) .data$.group else NULL
  )) +
    ggplot2::geom_col(position = bar_position)

  # Labels
  if (show_labels) {
    if (stacked) {
      p <- p + ggplot2::geom_text(
        data = data_labels,
        ggplot2::aes(label = round(.y, 1)),
        position = ggplot2::position_stack(vjust = 0.5),
        size = label_size
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = data_labels,
        ggplot2::aes(label = round(.y, 1)),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.3,
        size = label_size
      )
    }
  }

  # Labels, title, caption
  p <- p +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = legend_title,
      caption = note
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.direction = legend_direction
    )

  # Facet
  if (".facet" %in% colnames(data_summary)) {
    p <- p + ggplot2::facet_wrap(~.facet, scales = facet_scales, ncol = facet_ncol)
  }

  # Hide legend if no group
  if (!(".group" %in% colnames(data_summary))) p <- p + ggplot2::guides(fill = "none")

  # Flip coordinates
  if (horizontal) p <- p + ggplot2::coord_flip()

  return(p)
}
