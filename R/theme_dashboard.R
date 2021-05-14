#' @export
get_dashboard_theme <- function() {
  create_theme() %>%
    add_style("hintergrund", color = "#dddddd", background = "#dddddd") %>%
    add_style("text", fontfamily = "Arial", fontsize = 10) %>%
    add_style("textbold", base = "text", fontfamily = "Arial Black") %>%
    add_style("titelbalken", base = "textbold", fontsize = 24, color = "#ffffff", background = zhcolors$blau, valign = "center", padding = c(2, 2, 0.5, 2)) %>%
    add_style("spaltentitel", base = "textbold", fontsize = 20, color = zhcolors$blau, background = "#dddddd", padding = c(2, 2, 0.5, 2), margin_top = 0.3) %>%
    add_style("plot", margin = c(2, 0, 0, 2)) %>%
    add_style("value_box_value", base = "textbold", fontsize = 20, color = "#ffffff", background = zhcolors$blau, padding = c(5, 5, 2, 5), margin = c(2, 0, 0, 2)) %>%
    add_style("value_box_text", base = "text", fontsize = 14, color = "#ffffff", background = zhcolors$blau, padding = c(2, 5, 5, 5), margin = c(0, 0, 0, 2)) %>%
    add_style("value_box_value_success", base = "value_box_value", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_text_success", base = "value_box_text", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_value_warning", base = "value_box_value", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_text_warning", base = "value_box_text", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_value_danger", base = "value_box_value", background = zhcolors$rot) %>%
    add_style("value_box_text_danger", base = "value_box_text", background = zhcolors$rot)
}
