#' @export
get_slide_theme <- function() {
  create_theme() %>%
    add_style("zhblau", color = zhcolors$cyan, background = zhcolors$cyan) %>%
    add_style("blackborder", color = "#000000", background = NA) %>%
    add_style("text", fontfamily = "Arial", fontsize = 10) %>%
    add_style("textbold", base = "text", fontfamily = "Arial Black") %>%
    add_style("textboldblue", base = "textbold", color = "#FFFFFF", background = zhcolors$blau, padding = c(1, 1, 1, 1)) %>%
    add_style("textboldblueright", base = "textboldblue", halign = "right") %>%
    add_style("textboldgray", base = "textbold", background = "#CCCCCC", halign = "right", padding = c(1, 1, 1, 1)) %>%
    add_style("textgray", base = "text", background = "#CCCCCC", padding = c(1, 1, 1, 1)) %>%
    add_style("textboldblue2", base = "textbold", fontsize = 9, color = "#FFFFFF", background = zhcolors$blau, padding = c(0.5, 0.5, 0.5, 0.5)) %>%
    add_style("textboldblueright2", base = "textboldblue2", halign = "right") %>%
    add_style("textboldgray2", base = "textbold", fontsize = 9, background = "#CCCCCC", halign = "right", padding = c(0.5, 0.5, 0.5, 0.5)) %>%
    add_style("textgray2", base = "text", fontsize = 9, background = "#CCCCCC", padding = c(0.5, 0.5, 0.5, 0.5)) %>%
    add_style("pagenr", fontfamily = "Arial", fontsize = 9) %>%
    add_style("pagenrbold", base = "pagenr", fontfamily = "Arial Black") %>%
    add_style("hdrtext", base = "text", fontsize = 13) %>%
    add_style("hdrtextbold", base = "hdrtext", fontfamily = "Arial Black") %>%
    add_style("maintitle", fontfamily = "Arial Black", fontsize = 24) %>%
    add_style("mainsubtitle", base = "maintitle", fontsize = 16) %>%
    add_style("title", base = "textbold", fontsize = 16) %>%
    add_style("subtitle", base = "title", fontsize = 13) %>%
    add_style("annotate_L", fontfamily = "Arial", halign = "right", fontsize = 7) %>%
    add_style("annotate_M", fontfamily = "Arial", halign = "right", fontsize = 5) %>%
    add_style("spaltentitel", base = "textbold", fontsize = 8, background = "#ffffff", padding = c(1, 1, 0.25, 1), margin_top = 0.3) %>%
    add_style("value_box_value", base = "textbold", fontsize = 8, color = "#ffffff", background = zhcolors$blau, padding = c(2, 2, 1, 2), margin = c(1, 0, 0, 1)) %>%
    add_style("value_box_text", base = "text", fontsize = 6, color = "#ffffff", background = zhcolors$blau, padding = c(1, 2, 2, 2), margin = c(0, 0, 0, 1)) %>%
    add_style("value_box_value_success", base = "value_box_value", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_text_success", base = "value_box_text", background = zhcolors$dunkelgruen) %>%
    add_style("value_box_value_warning", base = "value_box_value", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_text_warning", base = "value_box_text", background = zhcolors$dunkelgelb) %>%
    add_style("value_box_value_danger", base = "value_box_value", background = zhcolors$rot) %>%
    add_style("value_box_text_danger", base = "value_box_text", background = zhcolors$rot)
}
