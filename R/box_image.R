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
add_image_box <- function(obj, image, style_id, rect) {
  page <- get_page(obj)
  page$boxes[[next_box_idx(page)]] <- create_image_box(image, style_id, rect)
  update_page(obj, page)
}

#' @export
render_box.image_box <- function(box, page) {
  style <- page$theme[[box$style_id]]
  grid::grid.raster(
    box$image,
    x = grid::unit(box$x + style$margin_left, "mm"),
    y = grid::unit(page$height - box$y - style$margin_top, "mm"),
    width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
    height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
    hjust = 0,
    vjust = 1)
}
