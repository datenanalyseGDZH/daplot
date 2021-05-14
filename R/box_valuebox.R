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
