#' Bar chart component
#'
#' Create a bar chart from raw data with optional grouping, faceting,
#' stacking, labels, and orientation. The component summarizes the data
#' internally and supports tidy-eval expressions.
#'
#' @export
bar_chart <- function(...) {
  bar_chart_impl <- geom_component(
    geom_layer   = ggplot2::geom_col,
    summarize_fn = summarize_xy,
    position_fn  = bar_position,
    label_fn     = bar_labels,
    supports_stack = TRUE
  )
  bar_chart_impl(...)
}



