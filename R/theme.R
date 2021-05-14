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
