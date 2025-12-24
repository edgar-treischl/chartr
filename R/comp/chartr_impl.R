# R/bar_chart_impl.R
bar_chart_impl <- geom_component(
  geom_layer   = ggplot2::geom_col,
  summarize_fn = summarize_xy,
  position_fn  = bar_position,
  label_fn     = bar_labels,
  supports_stack = TRUE
)
