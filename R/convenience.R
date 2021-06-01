#' @export
add_slide_pagenr <- function(obj, pagenr = NULL, x = NULL, y = NULL) {
  .Deprecated("set_slide_pagenr")
  set_slide_pagenr(obj, pagenr, x, y)
}

#' @export
set_slide_pagenr <- function(obj, pagenr = NULL, x = NULL, y = NULL) {
  page <- get_page(obj)
  if (is.null(x)) x <- page$width - 10
  if (is.null(y)) y <- 5
  x <- as_mm(x)
  y <- as_mm(y)
  if (is.null(pagenr)) pagenr <- page$pagenr
  page <- page %>%
    add_text_box(as.character(pagenr),
                 style_id = "pagenr",
                 rect = c(x, y, x + 7, y + 3))
  update_page(obj, page)
}

#' @export
add_slide_head <- function(obj, pagenr = NULL, x = NULL, y = NULL,
                           department = "Gesundheitsdirektion") {
  .Deprecated("set_slide_head")
  set_slide_head(obj, pagenr, x, y, directorate)
}

#' @export
set_slide_head <- function(obj, pagenr = NULL, x = NULL, y = NULL,
                           department = "Gesundheitsdirektion") {
  page <- get_page(obj)
  if (is.null(x)) x <- page$width - 55
  if (is.null(y)) y <- 7
  x <- as_mm(x)
  y <- as_mm(y)
  if (is.null(pagenr)) pagenr <- page$pagenr
  page <- page %>%
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
    add_text_box(paste0("Seite ", pagenr),
                 style_id = "pagenr",
                 rect = c(x + 7, y - 2 + 6, x + 7 + 50, y - 2 + 9))
  update_page(obj, page)
}

#' @export
add_slide_title <- function(obj, title, x = 10, y = 10) {
  .Deprecated("set_slide_title")
  set_slide_title(obj, title, x, y)
}

#' @export
set_slide_title <- function(obj, title, x = 10, y = 10) {
  page <- get_page(obj)
  x <- as_mm(x)
  y <- as_mm(y)
  page <- page %>%
    add_text_box(title,
                 style_id = "title",
                 rect = c(x, y, 160 - 2 * x, x + 7))
  update_page(obj, page)
}

#' @export
add_slide_subtitle <- function(obj, subtitle, x = 10, y = 17) {
  .Deprecated("set_slide_subtitle")
  set_slide_subtitle(obj, subtitle, x, y)
}

#' @export
set_slide_subtitle <- function(obj, subtitle, x = 10, y = 17) {
  page <- get_page(obj)
  x <- as_mm(x)
  y <- as_mm(y)
  page <- page %>%
    add_text_box(subtitle,
                 style_id = "subtitle",
                 rect = c(x, y, page$width - 2 * x, x + 7))
  update_page(obj, page)
}

#' @export
add_slide_source <- function(obj, text, x = 0, y = 110) {
  .Deprecated("set_slide_source")
  set_slide_source(obj, text, x, y)
}

#' @export
set_slide_source <- function(obj, text, x = 0, y = 110) {
  page <- get_page(obj)
  x <- as_mm(x)
  y <- as_mm(y)
  page <- page %>%
    add_text_box(text,
                 style_id = "annotate_L",
                 rect = c(x, y, x + 150, y + 7))
  update_page(obj, page)
}
