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

#' Get a team-standard color palette
#'
#' @param palette_name Character. Name of the palette (categorical, sequential, divergent, etc.)
#' @param n Integer. Number of colors required
#' @param fixed_mapping Named vector of colors for specific groups (optional)
#' @return Character vector of hex colors
#' @examples
#' color_picker("categorical", n = 3)
#' color_picker(fixed_mapping = c(A = "#B3CDE3", B = "#1F77B4"))
#' @export
#'
color_picker <- function(palette_name = NULL, n = NULL, fixed_mapping = NULL) {




  # Internal palettes
  palettes <- list(
    categorical = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD"),
    sequential  = c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#3182BD", "#08519C"),
    divergent   = c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#E0F3F8", "#ABD9E9", "#4575B4")
  )

  if (fixed_mapping) {
    palettes <- list(
      fixed_colors = c(A = "#B3CDE3", B = "#1F77B4")
    )

  }

  # Fixed mapping mode
  if (!is.null(fixed_mapping)) {
    if (is.null(names(fixed_mapping)) || any(names(fixed_mapping) == "")) {
      stop("fixed_mapping must be a named vector: names define the group mapping.")
    }
    return(fixed_mapping)
  }

  # Palette mode
  if (is.null(palette_name)) {
    stop("Either palette_name or fixed_mapping must be provided.")
  }

  if (!palette_name %in% names(palettes)) {
    stop("Palette '", palette_name, "' not found. Available palettes: ", paste(names(palettes), collapse = ", "))
  }

  pal <- palettes[[palette_name]]

  if (!is.null(n)) {
    if (n > length(pal)) {
      stop("Requested ", n, " colors but palette '", palette_name, "' only has ", length(pal), " colors.")
    }
    pal <- pal[seq_len(n)]
  }

  pal
}

