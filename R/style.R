default_style <- structure(list(
  fontfamily = "Arial",
  fontsize = 10,
  color = "black",
  background = NULL,
  halign = "left",
  valign = "top",
  padding_top = 0,
  padding_right = 0,
  padding_bottom = 0,
  padding_left = 0,
  margin_top = 0,
  margin_right = 0,
  margin_bottom = 0,
  margin_left = 0
), class = "style")

style <- function(base = default_style,
                  fontfamily = NULL, 
                  fontsize = NULL, 
                  halign = NULL, 
                  valign = NULL, 
                  color = NULL, 
                  background = NULL,
                  padding_top = NULL,
                  padding_right = NULL,
                  padding_bottom = NULL,
                  padding_left = NULL,
                  margin_top = NULL,
                  margin_right = NULL,
                  margin_bottom = NULL,
                  margin_left = NULL) {
  result <- base
  if (!is.null(fontfamily)) result$fontfamily <- fontfamily
  if (!is.null(fontsize)) result$fontsize <- fontsize
  if (!is.null(halign)) result$halign = halign
  if (!is.null(valign)) result$valign = valign
  if (!is.null(color)) result$color = color
  if (!is.null(background)) result$background = background
  if (!is.null(padding_top)) result$padding_top = padding_top
  if (!is.null(padding_right)) result$padding_right = padding_right
  if (!is.null(padding_bottom)) result$padding_bottom = padding_bottom
  if (!is.null(padding_left)) result$padding_left = padding_left
  if (!is.null(margin_top)) result$margin_top = margin_top
  if (!is.null(margin_right)) result$margin_right = margin_right
  if (!is.null(margin_bottom)) result$margin_bottom = margin_bottom
  if (!is.null(margin_left)) result$margin_left = margin_left
  result
}
