#' @export
render_box <- function(box, static_plot) {
  UseMethod("render_box")
}

#' @export
render_box.text_box <- function(box, static_plot) {
  style <- static_plot$theme[[box$style_id]]
  if (!is.null(style$background)) {
    grid::grid.rect(
      x = grid::unit(box$x + style$margin_left, "mm"),
      y = grid::unit(static_plot$height - box$y - style$margin_top, "mm"),
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
    y <- grid::unit(static_plot$height - box$y - style$padding_top - style$margin_top, "mm")
    just[2] <- "top"
  } else if (style$valign == "bottom") {
    y <- grid::unit(static_plot$height - box$y - box$height + style$padding_bottom + style$margin_bottom, "mm")
    just[2] <- "bottom"
  } else {
    y <- grid::unit(static_plot$height - box$y - box$height / 2, "mm")
    just[2] <- "center"
  }
  grid::grid.text(
    box$text, x, y, just,
    gp = grid::gpar(fontfamily = style$fontfamily,
              fontsize = style$fontsize,
              col = style$color))
}

#' @export
render_box.multi_line_text_box <- function(box, static_plot) {
  .NotYetImplemented()
}

#' @export
render_box.plot_box <- function(box, static_plot) {
  style <- static_plot$theme[[box$style_id]]
  grid::pushViewport(
    grid::viewport(x = grid::unit(box$x + style$margin_left, "mm"),
                   y = grid::unit(static_plot$height - box$y - style$margin_top, "mm"),
                   width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
                   height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
                   just = c(0, 1)))
  grid::grid.draw(ggplot2::ggplotGrob(box$plot))
  grid::popViewport()
}

#' @export
render_box.image_box <- function(box, static_plot) {
  style <- static_plot$theme[[box$style_id]]
  grid::grid.raster(
    box$image,
    x = grid::unit(box$x + style$margin_left, "mm"),
    y = grid::unit(static_plot$height - box$y - style$margin_top, "mm"),
    width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
    height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
    hjust = 0,
    vjust = 1)
}

#' @export
render_box.polygon_box <- function(box, static_plot) {
  style <- static_plot$theme[[box$style_id]]
  grid::grid.polygon(
    box$points_x + style$margin_right,
    static_plot$height - box$points_y - style$margin_top,
    default.units = "mm",
    gp = grid::gpar(fill = style$background, col = style$color))
}

#' @export
render_box.rect_box <- function(box, static_plot) {
  style <- static_plot$theme[[box$style_id]]
  grid::grid.rect(
    x = grid::unit(box$x + style$margin_left, "mm"),
    y = grid::unit(static_plot$height - box$y - style$margin_top, "mm"),
    width = grid::unit(box$width - style$margin_left - style$margin_right, "mm"),
    height = grid::unit(box$height - style$margin_top - style$margin_bottom, "mm"),
    hjust = 0,
    vjust = 1,
    gp = grid::gpar(col = style$color, fill = style$background))
}


#' @export
save_as <- function(static_plot, file, res = 600) {
  # TODO support PDF, SVG, PS etc in addition to PNG
  grDevices::png(
    paste0(file, ".png"),
    width = static_plot$width,
    height = static_plot$height,
    units = "mm",
    res = res)
  for (box in static_plot$boxes) {
    render_box(box, static_plot)
  }
  grDevices::dev.off()
}
