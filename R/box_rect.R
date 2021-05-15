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

#' @export
add_rect_box <- function(obj, rect, style_id) {
  page <- get_page(obj)
  page$boxes[[next_box_idx(page)]] <- create_rect_box(rect, style_id)
  update_page(obj, page)
}

#' @export
render_box.rect_box <- function(box, page) {
  style <- get_style(page$theme, box$style_id)
  grid::grid.rect(
    x = grid::unit(box$x + style$margin_left, "mm"),
    y = grid::unit(page$height - box$y - style$margin_top, "mm"),
    width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
    height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
    hjust = 0,
    vjust = 1,
    gp = grid::gpar(col = style$color, fill = style$background))
}
