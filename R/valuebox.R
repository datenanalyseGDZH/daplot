#' @export
append_value_box <- function(obj, value, text,
                             x = NULL, y = NULL, width = NULL,
                             level = "default",
                             value_style_id = "value_box_value",
                             text_style_id = "value_box_text") {
  page <- get_page(obj)
  if (!is.null(x)) x <- as_mm(x)
  if (!is.null(y)) y <- as_mm(y)
  if (!is.null(width)) width <- as_mm(width)
  if (level != "default") {
    value_style_id <- paste0(value_style_id, "_", level)
    text_style_id <- paste0(text_style_id, "_", level)
  }
  if (!is.null(x)) page$pos_x <- x
  if (!is.null(y)) page$pos_y <- y
  page <- page %>%
    append_text_box(value, value_style_id, width = width) %>%
    append_text_box(text, text_style_id)
  update_page(obj, page)
}

#' @export
append_value_box2 <- function(obj, value1, value2, text, suffix = "",
                              x = NULL, y = NULL, width = NULL,
                              level = "default",
                              value_style_id = "value_box_value",
                              text_style_id = "value_box_text") {
  page <- get_page(obj)
  value <- paste0(format(value1, big.mark = "'"), " / ", format(value2, big.mark = "'"), " ", suffix)
  page <- page %>%
    append_value_box(value, text, x, y, width, level, value_style_id, text_style_id)
  update_page(obj, page)
}

#' @export
append_value_box_delta <- function(obj, value, delta, text,
                                   x = NULL, y = NULL, width = NULL,
                                   level = "default",
                                   value_style_id = "value_box_value",
                                   text_style_id = "value_box_text") {
  page <- get_page(obj)
  value <- paste0(format(value, big.mark = "'"),
                  " (", ifelse(delta >= 0, "+", ""), format(delta, big.mark = "'"), ")")
  page <- page %>%
    append_value_box(value, text, x, y, width, level, value_style_id, text_style_id)
  update_page(obj, page)
}
