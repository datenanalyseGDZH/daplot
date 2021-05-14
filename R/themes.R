#' @export
create_theme <- function() {
  structure(list(
    default = default_style
  ), class = "theme")
}

#' @export
add_style <- function(theme,
                      name,
                      base = NULL,
                      fontfamily = NULL,
                      fontsize = NULL,
                      halign = NULL,
                      valign = NULL,
                      color = NULL,
                      background = NULL,
                      padding_top = NULL,
                      padding_right = NULL,
                      padding_bottom = NULL,
                      padding_left = NULL,
                      padding = NULL,
                      margin_top = NULL,
                      margin_right = NULL,
                      margin_bottom = NULL,
                      margin_left = NULL,
                      margin = NULL) {
  if (is.null(base)) {
    base <- default_style
  } else {
    base <- theme[[base]]
  }
  if (!is.null(padding)) {
    padding_top <- padding[1]
    padding_right <- padding[2]
    padding_bottom <- padding[3]
    padding_left <- padding[4]
  }
  if (!is.null(margin)) {
    margin_top <- margin[1]
    margin_right <- margin[2]
    margin_bottom <- margin[3]
    margin_left <- margin[4]
  }
  theme[[name]] <- style(base = base,
                         fontfamily = fontfamily,
                         fontsize = fontsize,
                         halign = halign,
                         valign = valign,
                         color = color,
                         background = background,
                         padding_top = padding_top,
                         padding_right = padding_right,
                         padding_bottom = padding_bottom,
                         padding_left = padding_left,
                         margin_top = margin_top,
                         margin_right = margin_right,
                         margin_bottom = margin_bottom,
                         margin_left = margin_left)
  theme
}

#' @export
get_merged_property <- function(theme, name, base1, base2, base3, base4, base5, default) {
  result <- theme[[base1]]$name
  if (is.null(result)) result <- theme[[base2]]$name
  if (is.null(result) && !is.null(base3)) result <- theme[[base3]]$name
  if (is.null(result) && !is.null(base4)) result <- theme[[base4]]$name
  if (is.null(result) && !is.null(base5)) result <- theme[[base5]]$name
  if (is.null(result)) result <- default
  result
}

#' @export
add_merged_style <- function(theme,
                      name,
                      base1,
                      base2,
                      base3 = NULL,
                      base4 = NULL,
                      base5 = NULL,
                      fontfamily = NULL,
                      fontsize = NULL,
                      halign = NULL,
                      valign = NULL,
                      color = NULL,
                      background = NULL,
                      padding_top = NULL,
                      padding_right = NULL,
                      padding_bottom = NULL,
                      padding_left = NULL,
                      padding = NULL,
                      margin_top = NULL,
                      margin_right = NULL,
                      margin_bottom = NULL,
                      margin_left = NULL,
                      margin = NULL) {
  theme[[name]] <- style(fontfamily     = get_merged_property(theme, "fontfamily", base1, base2, base3, base4, base5, fontfamily),
                         fontsize       = get_merged_property(theme, "fontsize", base1, base2, base3, base4, base5, fontsize),
                         halign         = get_merged_property(theme, "halign", base1, base2, base3, base4, base5, halign),
                         valign         = get_merged_property(theme, "valign", base1, base2, base3, base4, base5, valign),
                         color          = get_merged_property(theme, "color", base1, base2, base3, base4, base5, color),
                         background     = get_merged_property(theme, "background", base1, base2, base3, base4, base5, background),
                         padding_top    = get_merged_property(theme, "padding_top", base1, base2, base3, base4, base5, padding_top),
                         padding_right  = get_merged_property(theme, "padding_right", base1, base2, base3, base4, base5, padding_right),
                         padding_bottom = get_merged_property(theme, "padding_bottom", base1, base2, base3, base4, base5, padding_bottom),
                         padding_left   = get_merged_property(theme, "padding_left", base1, base2, base3, base4, base5, padding_left),
                         margin_top     = get_merged_property(theme, "margin_top", base1, base2, base3, base4, base5, margin_top),
                         margin_right   = get_merged_property(theme, "margin_right", base1, base2, base3, base4, base5, margin_right),
                         margin_bottom  = get_merged_property(theme, "margin_bottom", base1, base2, base3, base4, base5, margin_bottom),
                         margin_left    = get_merged_property(theme, "margin_left", base1, base2, base3, base4, base5, margin_left))
  theme
}

#' @export
get_style <- function(theme, name) {
  theme[[name]]
}

#' @export
get_default_theme <- function() {
  create_theme() %>%
    add_style("text", fontfamily = "Arial", fontsize = 10) %>%
    add_style("title", base = "text", fontfamily = "Arial Black", fontsize = 22) %>%
    add_style("subtitle", base = "title", fontsize = 13)
}

#' @export
get_powerpoint_theme <- function() {
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
