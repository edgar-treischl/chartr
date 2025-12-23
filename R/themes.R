#' Theme for mcplots
#'
#' @returns A ggplot2 theme object
#' @export
#'
theme_mc <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      # Grid lines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.4),

      # Axis lines and ticks
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.text = ggplot2::element_text(color = "#374151"),

      # Axis titles
      axis.title.x = ggplot2::element_text(color = "#374151", margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(color = "#374151", margin = ggplot2::margin(r = 8)),

      # Plot title
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2, margin = ggplot2::margin(b = 8)),
      plot.title.position = "plot",

      # Legend
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )
}


