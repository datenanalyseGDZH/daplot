#' @export
create_document <- function(title = "unnamed", width = NULL, height = NULL, format = NULL, theme = default_theme()) {
  structure(list(
    title = title,
    width = width,
    height = height,
    format = format,
    theme = theme,
    pages = list(),
    current_page_number = 0,
    current_section_number = 0,
    current_section = NULL,
    current_page_number_in_section = 0
  ), class = "document")
}

#' @export
get_page <- function(obj) {
  UseMethod("get_page")
}

#' @export
get_page.document <- function(doc) {
  doc$pages[[paste0("page", length(doc$pages))]]
}

#' @export
update_page <- function(obj, page) {
  UseMethod("update_page")
}

#' @export
update_page.document <- function(doc, page) {
  doc$pages[[paste0("page", length(doc$pages))]] <- page
  doc
}

#' @export
next_page_idx <- function(doc) {
  paste0("page", length(doc$pages) + 1)
}

#' @export
start_page <- function(doc) {
  if (is.null(doc$current_section) && doc$current_section_number == 0) {
    doc$current_section_number <- 1
  }
  doc$current_page_number <- doc$current_page_number + 1
  doc$current_page_number_in_section <- doc$current_page_number_in_section + 1
  current_page <- create_page(doc$width, doc$height, doc$format, doc$theme)
  current_page$doctitle <- doc$title
  current_page$pagenr <- doc$current_page_number
  current_page$section_nr <- doc$current_section_number
  current_page$section <- doc$current_section
  current_page$pagenr_in_section <- doc$current_page_number_in_section
  doc$pages[[next_page_idx(doc)]] <- current_page
  doc
}

#' @export
start_section <- function(doc, section, section_number = NULL) {
  doc$current_section <- section
  if (!is.null(section_number)) {
    doc$current_section_number <- section_number
  } else {
    doc$current_section_number <- doc$current_section_number + 1
  }
  doc$current_page_number_in_section <- 0
  doc
}

#' @export
save_pdf <- function(doc, file = NULL) {
  extrafont::loadfonts(device = "pdf", quiet = TRUE)
  if (class(doc) == "document") {
    grDevices::cairo_pdf(filename = ifelse(is.null(file), paste0(doc$title, ".pdf"), paste0(file, ".pdf")),
                 width = doc$pages$page1$width / 25.4,
                 height = doc$pages$page1$height / 25.4,
                 family = "Arial",
                 onefile = TRUE)
    pagenr <- 1
    for (page in doc$pages) {
      for (box in page$boxes) {
        render_box(box, page)
      }
      if (pagenr < length(doc$pages)) {
        grid::grid.newpage()
      }
      pagenr <- pagenr + 1
    }
  } else {
    page <- doc
    grDevices::cairo_pdf(filename = paste0(file, ".pdf"),
                 width = page$width / 25.4,
                 height = page$height / 25.4,
                 family = "Arial",
                 onefile = TRUE)
    for (box in page$boxes) {
      render_box(box, page)
    }
  }
  grDevices::dev.off()
}
