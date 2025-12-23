mc_palette <- c(
  "#2563eb", "#dc2626", "#16a34a",
  "#f59e0b", "#7c3aed"
)

scale_color_mc <- function(...) {
  ggplot2::scale_color_manual(values = mc_palette, ...)
}

scale_fill_mc <- function(...) {
  ggplot2::scale_fill_manual(values = mc_palette, ...)
}
