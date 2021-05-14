#' @export
next_box_idx <- function(static_plot) {
  paste0("box", length(static_plot$boxes) + 1)
}

#' @export
add_text_box <- function(static_plot, text, style_id, rect) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_text_box(text, style_id, rect)
  static_plot
}

#' @export
add_multi_line_text_box <- function(static_plot, text, style_id, rect) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_multi_line_text_box(text, style_id, rect)
  static_plot
}

#' @export
add_plot_box <- function(static_plot, plot, style_id, rect) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_plot_box(plot, style_id, rect)
  static_plot
}

#' @export
add_image_box <- function(static_plot, image, style_id, rect) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_image_box(image, style_id, rect)
  static_plot
}

#' @export
add_polygon_box <- function(static_plot, points_x, points_y, style_id) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_polygon_box(points_x, points_y, style_id)
  static_plot
}

#' @export
add_rect_box <- function(static_plot, rect, style_id) {
  static_plot$boxes[[next_box_idx(static_plot)]] <- create_rect_box(rect, style_id)
  static_plot
}
