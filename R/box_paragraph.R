#' @export
create_paragraph_box <- function(text, style_id, rect) {
  structure(list(
    text = text,
    style_id = style_id,
    x = rect[1],
    y = rect[2],
    width = rect[3] - rect[1],
    height = rect[4] - rect[2]
  ), class = "paragraph_box")
}

#' @export
add_paragraph_box <- function(obj, text, style_id, rect) {
  page <- get_page(obj)
  page$boxes[[next_box_idx(page)]] <- create_paragraph_box(text, style_id, rect)
  update_page(obj, page)
}

#' @export
render_box.paragraph_box <- function(box, page) {
  .NotYetImplemented()
}
