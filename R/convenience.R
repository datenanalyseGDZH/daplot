#' @export
add_folien_nr <- function(static_plot, foliennr, x = static_plot$width - 55, y = 7) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    add_polygon_box(points_x = c(x, x + 5, x),
                    points_y = c(y + 5, y + 5, y),
                    style_id = "zhblau") %>%
    add_rect_box(rect = c(x, y, x + 5, y + 5), style_id = "blackborder") %>%
    add_text_box("Kanton Zürich",
                 style_id = "pagenr",
                 rect = c(x + 7, y - 2, x + 7 + 50, y - 2 + 3)) %>%
    add_text_box("Gesundheitsdirektion",
                 style_id = "pagenrbold",
                 rect = c(x + 7, y - 2 + 3, x + 7 + 50, y - 2 + 6)) %>%
    add_text_box(paste0("Seite ", foliennr),
                 style_id = "pagenr",
                 rect = c(x + 7, y - 2 + 6, x + 7 + 50, y - 2 + 9))
}

#' @export
add_folien_kopf_kapo <- function(static_plot, x = 10, y = 7) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    add_polygon_box(points_x = c(x, x + 5, x),
                    points_y = c(y + 5, y + 5, y),
                    style_id = "zhblau") %>%
    add_rect_box(rect = c(x, y, x + 5, y + 5), style_id = "blackborder") %>%
    add_text_box("Kanton Zürich",
                 style_id = "pagenr",
                 rect = c(x + 7, y - 0.5, x + 7 + 50, y - 0.5 + 3)) %>%
    add_text_box("Kantonale Führungsorganisation",
                 style_id = "pagenrbold",
                 rect = c(x + 7, y - 0.5 + 3, x + 7 + 50, y - 0.5 + 6))
}

#' @export
add_folien_titel <- function(static_plot, titel, x = 10, y = 10) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    add_text_box(titel,
                 style_id = "title",
                 rect = c(x, y, 160 - 2 * x, x + 7))
}

#' @export
add_folien_subtitel <- function(static_plot, subtitel, x = 10, y = 17) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    add_text_box(subtitel,
                 style_id = "subtitle",
                 rect = c(x, y, static_plot$width - 2 * x, x + 7))
}

#' @export
add_folien_source <- function(static_plot, x = 0, y = 110) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    set_pos(x = x, y = y) %>%
    append_text_box(paste0("Quelle: GD, Stand ", heute_fix_15), "annotate_L", width = 150)
}

#' @export
add_folien_source_bag <- function(static_plot, x = 0, y = 110) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    set_pos(x = x, y = y) %>%
    append_text_box(paste0("Quelle: GD und BAG"), "annotate_L", width = 150)
}

#' @export
add_folien_source_noDate <- function(static_plot, x = 0, y = 110) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    set_pos(x = x, y = y) %>%
    append_text_box(paste0("Quelle: GD"), "annotate_L", width = 150)
}

#' @export
add_folien_source_eth <- function(static_plot, x = 0, y = 110) {
  x <- as_mm(x)
  y <- as_mm(y)
  static_plot %>%
    set_pos(x = x, y = y) %>%
    append_text_box(paste0("Quelle: https://ibz-shiny.ethz.ch/covid-19-re/"), "annotate_L", width = 150)
}

#' @export
append_value_box2 <- function(static_plot, value1, value2, text, suffix = "", x = NULL, y = NULL, width = NULL, level = "default",
                              value_style_id = "value_box_value", text_style_id = "value_box_text") {
  static_plot %>%
    append_value_box(paste0(format(value1, big.mark = "'"), " / ", format(value2, big.mark = "'"), " ", suffix),
                     text, x, y, width, level, value_style_id, text_style_id)
}

#' @export
append_value_box_delta <- function(static_plot, value, delta, text, x = NULL, y = NULL, width = NULL, level = "default",
                              value_style_id = "value_box_value", text_style_id = "value_box_text") {
  static_plot %>%
    append_value_box(paste0(format(value, big.mark = "'"),
                            " (",
                            ifelse(delta >= 0, "+", ""),
                            format(delta, big.mark = "'"),
                            ")"),
                     text, x, y, width, level, value_style_id, text_style_id)
}
