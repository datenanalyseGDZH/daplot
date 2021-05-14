#' @export
set_pos <- function(static_plot, x = NULL, y = NULL, x_end = NULL) {
  if (!is.null(x)) x <- as_mm(x)
  else x <- static_plot$pos_x
  if (!is.null(y)) y <- as_mm(y)
  else y <- static_plot$pos_y
  if (!is.null(x_end)) x_end <- as_mm(x_end)
  else x_end <- static_plot$pos_x_end
  static_plot$pos_x_start <- x
  static_plot$pos_x <- x
  static_plot$pos_y <- y
  static_plot$pos_x_end <- x_end
  static_plot
}

#' @export
store_current_y_pos <- function(static_plot) {
  static_plot$pos_y_saved <- static_plot$pos_y
  static_plot
}

#' @export
store_current_x_pos <- function(static_plot) {
  static_plot$pos_x_saved <- static_plot$pos_x
  static_plot
}

#' @export
last_box_height <- function(static_plot) {
  last_box <- static_plot$boxes[[length(static_plot$boxes)]]
  last_box$height
}
