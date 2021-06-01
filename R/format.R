#' @export
get_format_dimensions <- function(format) {
  format <- tolower(format)
  if (format %in% c("a4", "a4hoch", "a4-hoch", "a4-portrait")) {
    list(width = 210, height = 297)
  } else if (format %in% c("a4quer", "a4q", "a4-quer", "a4landscape", "a4-landscape")) {
    list(width = 297, height = 210)
  } else if (format %in% c("a3quer", "a3q", "a3-quer", "a3landscape", "a3-landscape")) {
    list(width = 420, height = 297)
  } else if (format %in% c("a3", "a3hoch", "a3-hoch", "a3-portrait")) {
    list(width = 297, height = 420)
  } else if (format %in% c("dashboard", "infoboard")) {
    list(width = 420, height = 297)
  } else if (format %in% c("presentation", "powerpoint", "ppt", "pptx", "folie", "slide")) {
    list(width = 160, height = 120)
  } else if (format %in% c("slide_4x3", "folie_4x3")) {
    list(width = 160, height = 120)
  } else if (format %in% c("slide_16x9", "folie_16x9")) {
    list(width = 213, height = 120)
  } else {
    stop(paste0("unknown format ", format))
  }
}

#' @export
get_format_width <- function(format) {
  get_format_dimensions(format)$width
}

#' @export
get_format_height <- function(format) {
  get_format_dimensions(format)$height
}
