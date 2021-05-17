#' @export
create_polygon_box <- function(points_x, points_y, style_id) {
  structure(list(
    points_x = points_x,
    points_y = points_y,
    style_id = style_id
  ), class = "polygon_box")
}

#' @export
add_polygon_box <- function(obj, points_x, points_y, style_id) {
  page <- get_page(obj)
  page$boxes[[next_box_idx(page)]] <- create_polygon_box(points_x, points_y, style_id)
  update_page(obj, page)
}

#' @export
render_box.polygon_box <- function(box, page) {
  style <- get_style(page$theme, box$style_id)
  grid::grid.polygon(
    box$points_x + style$margin_right,
    page$height - box$points_y - style$margin_top,
    default.units = "mm",
    gp = grid::gpar(fill = style$background, col = style$color, lwd = page$lwd))
}
