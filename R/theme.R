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
  if (!is.null(base)) {
    base <- get_merged_style(theme, base)
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
get_merged_style_property <- function(theme, property, names) {
  result <- NULL
  for (name in names) {
    result <- theme[[name]][[property]]
    if (!is.null(result)) break
  }
  result
}

#' @export
get_merged_style <- function(theme, names) {
  style(fontfamily     = get_merged_style_property(theme, "fontfamily", names),
        fontsize       = get_merged_style_property(theme, "fontsize", names),
        halign         = get_merged_style_property(theme, "halign", names),
        valign         = get_merged_style_property(theme, "valign", names),
        color          = get_merged_style_property(theme, "color", names),
        background     = get_merged_style_property(theme, "background", names),
        padding_top    = get_merged_style_property(theme, "padding_top", names),
        padding_right  = get_merged_style_property(theme, "padding_right", names),
        padding_bottom = get_merged_style_property(theme, "padding_bottom", names),
        padding_left   = get_merged_style_property(theme, "padding_left", names),
        margin_top     = get_merged_style_property(theme, "margin_top", names),
        margin_right   = get_merged_style_property(theme, "margin_right", names),
        margin_bottom  = get_merged_style_property(theme, "margin_bottom", names),
        margin_left    = get_merged_style_property(theme, "margin_left", names))
}

#' @export
get_style <- function(theme, names) {
  style(base           = theme$default,
        fontfamily     = get_merged_style_property(theme, "fontfamily", names),
        fontsize       = get_merged_style_property(theme, "fontsize", names),
        halign         = get_merged_style_property(theme, "halign", names),
        valign         = get_merged_style_property(theme, "valign", names),
        color          = get_merged_style_property(theme, "color", names),
        background     = get_merged_style_property(theme, "background", names),
        padding_top    = get_merged_style_property(theme, "padding_top", names),
        padding_right  = get_merged_style_property(theme, "padding_right", names),
        padding_bottom = get_merged_style_property(theme, "padding_bottom", names),
        padding_left   = get_merged_style_property(theme, "padding_left", names),
        margin_top     = get_merged_style_property(theme, "margin_top", names),
        margin_right   = get_merged_style_property(theme, "margin_right", names),
        margin_bottom  = get_merged_style_property(theme, "margin_bottom", names),
        margin_left    = get_merged_style_property(theme, "margin_left", names))
}
