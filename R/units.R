#' @export
as_mm <- function(x) {
  if (class(x) == "unit") return(as.numeric(grid::convertUnit(x, "mm")))
  as.numeric(unit(x, "mm"))
}
