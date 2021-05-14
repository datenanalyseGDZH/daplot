#' @export
append_text_box <- function(static_plot, text, style_id, height = NULL, width = NULL) {
  style <- static_plot$theme[[style_id]]
  if (is.null(height)) {
    height <- style$fontsize * 25.4 / 72
    if (!is.null(style$padding_top)) height <- height + style$padding_top
    if (!is.null(style$padding_bottom)) height <- height + style$padding_bottom
  } else {
    height <- as_mm(height)
  }
  if (is.null(width)) {
    width <- static_plot$pos_x_end - static_plot$pos_x
  } else {
    width <- as_mm(width)
  }
  static_plot <- add_text_box(static_plot, text, style_id,
                              rect = c(static_plot$pos_x,
                                       static_plot$pos_y,
                                       static_plot$pos_x + width,
                                       static_plot$pos_y + height))
  static_plot$pos_x <- static_plot$pos_x + width
  if (static_plot$pos_x >= static_plot$pos_x_end) {
    static_plot <- append_new_line(static_plot)
  }
  static_plot
}

#' @export
append_plot_box <- function(static_plot, plot, height, width = NULL, style_id = "plot") {
  style <- static_plot$theme[[style_id]]
  height <- as_mm(height)
  if (is.null(width)) {
    width <- static_plot$pos_x_end - static_plot$pos_x
  } else {
    width <- as_mm(width)
  }
  static_plot <- static_plot %>%
    add_plot_box(plot, style_id,
                 rect = c(static_plot$pos_x,
                          static_plot$pos_y,
                          static_plot$pos_x + width,
                          static_plot$pos_y + height))
  static_plot$pos_x <- static_plot$pos_x + width
  if (static_plot$pos_x >= static_plot$pos_x_end) {
    static_plot <- append_new_line(static_plot)
  }
  static_plot
}

#' @export
append_value_box <- function(static_plot, value, text, x = NULL, y = NULL, width = NULL, level = "default",
                             value_style_id = "value_box_value", text_style_id = "value_box_text") {
  if (!is.null(x)) x <- as_mm(x)
  if (!is.null(y)) y <- as_mm(y)
  if (!is.null(width)) width <- as_mm(width)
  if (level != "default") {
    value_style_id <- paste0(value_style_id, "_", level)
    text_style_id <- paste0(text_style_id, "_", level)
  }
  if (!is.null(x)) static_plot$pos_x <- x
  if (!is.null(y)) static_plot$pos_y <- y
  static_plot %>%
    append_text_box(value, value_style_id, width = width) %>%
    append_text_box(text, text_style_id)
}

#' @export
append_new_line <- function(static_plot) {
  set_pos(static_plot,
          static_plot$pos_x_start,
          static_plot$pos_y + last_box_height(static_plot),
          static_plot$pos_x_end)
}
