#' @export
document <- function(width = NULL, height = NULL, format = NULL, theme = default_theme()) {
  structure(list(
    width = width,
    height = height,
    format = format,
    theme = theme,
    pages = list(),
    current_page_number = 1,
    current_section_number = 1,
    current_section = NULL,
    current_page_number_in_section = 1,
  ), class = "document")
}

# TODO add page
# TODO start section
# TODO render document
