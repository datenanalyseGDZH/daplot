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
add_text_box <- function(obj, text, style_id, rect) {
  page <- get_page(obj)
  page$boxes[[next_box_idx(page)]] <- create_text_box(text, style_id, rect)
  update_page(obj, page)
}

#' @export
append_text_box <- function(obj, text, style_id, height = NULL, width = NULL) {
  page <- get_page(obj)
  style <- page$theme[[style_id]]
  if (is.null(height)) {
    height <- style$fontsize * 25.4 / 72
    if (!is.null(style$padding_top)) height <- height + style$padding_top
    if (!is.null(style$padding_bottom)) height <- height + style$padding_bottom
  } else {
    height <- as_mm(height)
  }
  if (is.null(width)) {
    width <- page$pos_x_end - page$pos_x
  } else {
    width <- as_mm(width)
  }
  page <- add_text_box(page, text, style_id,
                              rect = c(page$pos_x,
                                       page$pos_y,
                                       page$pos_x + width,
                                       page$pos_y + height))
  page$pos_x <- page$pos_x + width
  if (page$pos_x >= page$pos_x_end) {
    page <- append_new_line(page)
  }
  update_page(obj, page)
}

#' @export
render_box.text_box <- function(box, page) {
  style <- page$theme[[box$style_id]]
  if (!is.null(style$background)) {
    grid::grid.rect(
      x = grid::unit(box$x + style$margin_left, "mm"),
      y = grid::unit(page$height - box$y - style$margin_top, "mm"),
      width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
      height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
      hjust = 0,
      vjust = 1,
      gp = grid::gpar(col = style$background, fill = style$background))
  }
  just <- c("", "")
  if (style$halign == "left") {
    x <- grid::unit(box$x + style$padding_left + style$margin_left, "mm")
    just[1] <- "left"
  } else if (style$halign == "right") {
    x <- grid::unit(box$x + box$width - style$padding_right - style$margin_right, "mm")
    just[1] <- "right"
  } else {
    x <- grid::unit(box$x + box$width / 2, "mm")
    just[1] <- "center"
  }
  if (style$valign == "top") {
    y <- grid::unit(page$height - box$y - style$padding_top - style$margin_top, "mm")
    just[2] <- "top"
  } else if (style$valign == "bottom") {
    y <- grid::unit(page$height - box$y - box$height + style$padding_bottom + style$margin_bottom, "mm")
    just[2] <- "bottom"
  } else {
    y <- grid::unit(page$height - box$y - box$height / 2, "mm")
    just[2] <- "center"
  }
  grid::grid.text(
    box$text, x, y, just,
    gp = grid::gpar(fontfamily = style$fontfamily,
              fontsize = style$fontsize,
              col = style$color))
}
