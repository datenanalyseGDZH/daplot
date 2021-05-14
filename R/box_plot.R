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
add_plot_box <- function(page, plot, style_id, rect) {
  page$boxes[[next_box_idx(page)]] <- create_plot_box(plot, style_id, rect)
  page
}

#' @export
append_plot_box <- function(page, plot, height, width = NULL, style_id = "plot") {
  style <- page$theme[[style_id]]
  height <- as_mm(height)
  if (is.null(width)) {
    width <- page$pos_x_end - page$pos_x
  } else {
    width <- as_mm(width)
  }
  page <- page %>%
    add_plot_box(plot, style_id,
                 rect = c(page$pos_x,
                          page$pos_y,
                          page$pos_x + width,
                          page$pos_y + height))
  page$pos_x <- page$pos_x + width
  if (page$pos_x >= page$pos_x_end) {
    page <- append_new_line(page)
  }
  page
}

#' @export
render_box.plot_box <- function(box, page) {
  style <- page$theme[[box$style_id]]
  grid::pushViewport(
    grid::viewport(x = grid::unit(box$x + style$margin_left, "mm"),
                   y = grid::unit(page$height - box$y - style$margin_top, "mm"),
                   width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
                   height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
                   just = c(0, 1)))
  grid::grid.draw(ggplot2::ggplotGrob(box$plot))
  grid::popViewport()
}
