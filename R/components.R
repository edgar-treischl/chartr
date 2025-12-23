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
    )
    #theme_mc()

  # Hide legend if group is NULL
  if (is.null(group)) {
    p <- p + ggplot2::guides(color = "none")
  }

  return(p)
}

