#' @export
get_default_theme <- function() {
  create_theme() %>%
    add_style("text", fontfamily = "Arial", fontsize = 10) %>%
    add_style("title", base = "text", fontfamily = "Arial Black", fontsize = 22) %>%
    add_style("subtitle", base = "title", fontsize = 13)
}
