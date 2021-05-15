#' @export
get_default_theme <- function() {
  create_theme() %>%
    add_style("right", halign = "right") %>%
    add_style("left", halign = "left") %>%
    add_style("center", halign = "center") %>%
    add_style("top", valign = "top") %>%
    add_style("bottom", valign = "bottom") %>%
    add_style("bold", fontfamily = "Arial Black") %>%
    add_style("text", fontfamily = "Arial", fontsize = 10) %>%
    add_style("title", base = c("bold", "text"), fontsize = 22) %>%
    add_style("subtitle", base = "title", fontsize = 13) %>%
    add_style("pagenr", base = "text", fontsize = 9) %>%
    add_style("annotate", base = c("right", "text"), fontsize = 7) %>%
    add_style("value_box_value", base = c("bold", "text"), fontsize = 8, color = "#ffffff", background = zhcolors$blau, padding = c(2, 2, 1, 2), margin = c(1, 0, 0, 1)) %>%
    add_style("value_box_text", base = "text", fontsize = 6, color = "#ffffff", background = zhcolors$blau, padding = c(1, 2, 2, 2), margin = c(0, 0, 0, 1)) %>%
    add_style("value_box_value_success", base = "value_box_value", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_text_success", base = "value_box_text", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_value_warning", base = "value_box_value", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_text_warning", base = "value_box_text", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_value_danger", base = "value_box_value", background = zhcolors$rot) %>%
    add_style("value_box_text_danger", base = "value_box_text", background = zhcolors$rot)
}
