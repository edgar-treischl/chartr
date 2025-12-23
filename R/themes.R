#' Theme for mcplots
#'
#' @returns A ggplot2 theme object
#' @export
#'
theme_mc <- function() {
  ggplot2::theme_minimal(
    base_family = get_mcplot_option("font_family"),
    base_size = get_mcplot_option("base_size")
  ) +
    ggplot2::theme(
      # Grid
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.4, color = "#e5e7eb"),

      # Axis lines & ticks
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks.length = grid::unit(3, "pt"),

      # Tick text
      axis.text = ggplot2::element_text(color = "#374151"),

      # Axis titles (visible)
      axis.title.x = ggplot2::element_text(color = "#374151"),
      axis.title.y = ggplot2::element_text(color = "#374151"),

      # Plot title (visible & styled)
      plot.title = ggplot2::element_text(
        face = "bold",
        size = get_mcplot_option("base_size") * 1.2
      ),
      plot.title.position = "plot",

      # Legend
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )
}

