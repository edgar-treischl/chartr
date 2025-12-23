mcplot_defaults <- list(
  font_family = "Inter",
  base_size = 12,
  palette = "mc_default"
)

set_mcplot_defaults <- function(...) {
  opts <- list(...)
  options(mcplot = modifyList(getOption("mcplot", mcplot_defaults), opts))
}

get_mcplot_option <- function(name) {
  getOption("mcplot", mcplot_defaults)[[name]]
}
