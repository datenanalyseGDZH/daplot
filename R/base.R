#' @export
static_plot <- function(width = NULL, height = NULL, format = NULL, theme = default_theme()) {
  if (!is.null(format)) {
    format <- tolower(format)
    if (format %in% c("a4", "a4hoch", "a4-hoch", "a4-portrait")) {
      width <- 210
      height <- 297
    } else if (format %in% c("a4quer", "a4q", "a4-quer", "a4landscape", "a4-landscape")) {
      width <- 297
      height <- 210
    } else if (format %in% c("a3quer", "a3q", "a3-quer", "a3landscape", "a3-landscape")) {
      width <- 420
      height <- 297
    } else if (format %in% c("a3", "a3hoch", "a3-hoch", "a3-portrait")) {
      width <- 297
      height <- 420
    } else if (format %in% c("dashboard", "infoboard")) {
      width <- 420
      height <- 297
    } else if (format %in% c("presentation", "powerpoint", "ppt", "pptx", "folie", "slide")) {
      width <- 160
      height <- 120
    }
  }
  stopifnot(!is.null(width) && !is.null(height))
  structure(list(
    width = as_mm(width),
    height = as_mm(height),
    boxes = list(),
    theme = theme,
    pos_x_start = 0,
    pos_x_end = as_mm(width),
    pos_x = 0,
    pos_y = 0,
    pos_x_saved = 0,
    pos_y_saved = 0
  ), class = "static_plot")
}

#' @export
create_text_box <- function(text, style_id, rect) {
  structure(list(
    text = text,
    style_id = style_id,
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2]
  ), class = "text_box")
}

#' @export
create_multi_line_text_box <- function(text, style_id, rect) {
  structure(list(
    text = text,
    style_id = style_id,
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2]
  ), class = "multi_line_text_box")
}

#' @export
create_plot_box <- function(plot, style_id, rect) {
  structure(list(
    plot = plot,
    style_id = style_id,
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2]
  ), class = "plot_box")
}

#' @export
create_image_box <- function(image, style_id, rect) {
  structure(list(
    image = image,
    style_id = style_id,
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2]
  ), class = "image_box")
}

#' @export
create_polygon_box <- function(points_x, points_y, style_id) {
  structure(list(
    points_x = points_x,
    points_y = points_y,
    style_id = style_id
  ), class = "polygon_box")
}

#' @export
create_rect_box <- function(rect, style_id) {
  structure(list(
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2],
    style_id = style_id
  ), class = "rect_box")
}

