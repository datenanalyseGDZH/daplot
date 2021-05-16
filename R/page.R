#' @export
create_page <- function(width = NULL, height = NULL, format = NULL,
                        theme = default_theme(),
                        pagenr = NULL) {
  if (!is.null(format)) {
    width <- get_format_width(format)
    height <- get_format_height(format)
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
    pos_y_saved = 0,
    doctitle = NULL,
    pagenr = pagenr,
    section_nr = NULL,
    section = NULL,
    pagenr_in_section = NULL
  ), class = "page")
}

#' @export
get_page.page <- function(page) {
  page
}

#' @export
update_page.page <- function(obj, page) {
  page
}

#' @export
width <- function(obj) {
  get_page(obj)$width
}

#' @export
height <- function(obj) {
  get_page(obj)$height
}

#' @export
pos_x <- function(obj) {
  get_page(obj)$pos_x
}

#' @export
pos_y <- function(obj) {
  get_page(obj)$pos_y
}

#' @export
pos_x_end <- function(obj) {
  get_page(obj)$pos_x_end
}

#' @export
pos_x_saved <- function(obj) {
  get_page(obj)$pos_x_saved
}

#' @export
pos_y_saved <- function(obj) {
  get_page(obj)$pos_y_saved
}

#' @export
next_box_idx <- function(page) {
  paste0("box", length(page$boxes) + 1)
}

#' @export
set_pos <- function(obj, x = NULL, y = NULL, x_end = NULL) {
  page <- get_page(obj)
  if (!is.null(x)) x <- as_mm(x)
  else x <- page$pos_x
  if (!is.null(y)) y <- as_mm(y)
  else y <- page$pos_y
  if (!is.null(x_end)) x_end <- as_mm(x_end)
  else x_end <- page$pos_x_end
  page$pos_x_start <- x
  page$pos_x <- x
  page$pos_y <- y
  page$pos_x_end <- x_end
  update_page(obj, page)
}

#' @export
store_current_y_pos <- function(obj) {
  page <- get_page(obj)
  page$pos_y_saved <- page$pos_y
  update_page(obj, page)
}

#' @export
store_current_x_pos <- function(obj) {
  page <- get_page(obj)
  page$pos_x_saved <- page$pos_x
  update_page(obj, page)
}

#' @export
last_box_height <- function(page) {
  last_box <- page$boxes[[length(page$boxes)]]
  last_box$height
}

#' @export
append_new_line <- function(page) {
  set_pos(page,
          page$pos_x_start,
          page$pos_y + last_box_height(page),
          page$pos_x_end)
}

#' @export
render_box <- function(box, page) {
  UseMethod("render_box")
}

#' @export
save_png <- function(page, file, res = 600) {
  extrafont::loadfonts(device = "win", quiet = TRUE)
  grDevices::png(
    paste0(file, ".png"),
    width = page$width,
    height = page$height,
    units = "mm",
    res = res)
  for (box in page$boxes) {
    render_box(box, page)
  }
  grDevices::dev.off()
}

#' @export
as_mm <- function(x) {
  if (class(x) == "unit") return(as.numeric(grid::convertUnit(x, "mm")))
  as.numeric(unit(x, "mm"))
}
