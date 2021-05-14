#' @export
add_slide_head <- function(page, foliennr,
                           directorate = "Gesundheitsdirektion",
                           x = page$width - 55, y = 7) {
  x <- as_mm(x)
  y <- as_mm(y)
  page %>%
    add_polygon_box(points_x = c(x, x + 5, x),
                    points_y = c(y + 5, y + 5, y),
                    style_id = "zhblau") %>%
    add_rect_box(rect = c(x, y, x + 5, y + 5), style_id = "blackborder") %>%
    add_text_box("Kanton Zürich",
                 style_id = "pagenr",
                 rect = c(x + 7, y - 2, x + 7 + 50, y - 2 + 3)) %>%
    add_text_box(directorate,
                 style_id = "pagenrbold",
                 rect = c(x + 7, y - 2 + 3, x + 7 + 50, y - 2 + 6)) %>%
    add_text_box(paste0("Seite ", foliennr),
                 style_id = "pagenr",
                 rect = c(x + 7, y - 2 + 6, x + 7 + 50, y - 2 + 9))
}

#' @export
add_slide_title <- function(page, title, x = 10, y = 10) {
  x <- as_mm(x)
  y <- as_mm(y)
  page %>%
    add_text_box(title,
                 style_id = "title",
                 rect = c(x, y, 160 - 2 * x, x + 7))
}

#' @export
add_slide_subtitle <- function(page, subtitle, x = 10, y = 17) {
  x <- as_mm(x)
  y <- as_mm(y)
  page %>%
    add_text_box(subtitle,
                 style_id = "subtitle",
                 rect = c(x, y, page$width - 2 * x, x + 7))
}

#' @export
add_slide_source <- function(page, text, x = 0, y = 110) {
  x <- as_mm(x)
  y <- as_mm(y)
  page %>%
    set_pos(x = x, y = y) %>%
    append_text_box(text, "annotate_L", width = 150)
}

#' @export
append_value_box2 <- function(page, value1, value2, text, suffix = "", x = NULL, y = NULL, width = NULL, level = "default",
                              value_style_id = "value_box_value", text_style_id = "value_box_text") {
  value <- paste0(format(value1, big.mark = "'"), " / ", format(value2, big.mark = "'"), " ", suffix)
  page %>%
    append_value_box(value, text, x, y, width, level, value_style_id, text_style_id)
}

#' @export
append_value_box_delta <- function(page, value, delta, text, x = NULL, y = NULL, width = NULL, level = "default",
                              value_style_id = "value_box_value", text_style_id = "value_box_text") {
  value <- paste0(format(value, big.mark = "'"),
                  " (", ifelse(delta >= 0, "+", ""), format(delta, big.mark = "'"), ")")
  page %>%
    append_value_box(value, text, x, y, width, level, value_style_id, text_style_id)
}
