#' @export
darker <- function(color) {
  color <- rgb2hsv(col2rgb(color))
  color[3] <- color[3] * 0.9
  hsv(color[1], color[2], color[3])
}

#' @export
lighter <- function(color) {
  color <- rgb2hsv(col2rgb(color))
  color[3] <- min(1.0, color[3] * 1.1)
  hsv(color[1], color[2], color[3])
}

#' @export
zhcolors = list(
  cyan    = rgb(  0, 158, 224, maxColorValue = 255),
  blau    = rgb(  0, 118, 189, maxColorValue = 255),
  violett = rgb(136,  94, 160, maxColorValue = 255),
  magenta = rgb(227,   0,  89, maxColorValue = 255),
  rot     = rgb(226,   0,  26, maxColorValue = 255),
  orange  = rgb(135, 105,  11, maxColorValue = 255),
  gelb    = rgb(255, 204,   0, maxColorValue = 255),
  tuerkis = rgb(  0, 161, 163, maxColorValue = 255),
  gruen   = rgb( 62, 167,  67, maxColorValue = 255)
)

zhcolors$hellcyan    <- lighter(zhcolors$cyan)
zhcolors$hellblau    <- lighter(zhcolors$blau)
zhcolors$hellviolett <- lighter(zhcolors$violett)
zhcolors$hellmagenta <- lighter(zhcolors$magenta)
zhcolors$hellrot     <- lighter(zhcolors$rot)
zhcolors$hellorange  <- lighter(zhcolors$orange)
zhcolors$hellgelb    <- lighter(zhcolors$gelb)
zhcolors$helltuerkis <- lighter(zhcolors$tuerkis)
zhcolors$hellgruen   <- lighter(zhcolors$gruen)

zhcolors$dunkelcyan    <- darker(zhcolors$cyan)
zhcolors$dunkelblau    <- darker(zhcolors$blau)
zhcolors$dunkelviolett <- darker(zhcolors$violett)
zhcolors$dunkelmagenta <- darker(zhcolors$magenta)
zhcolors$dunkelrot     <- darker(zhcolors$rot)
zhcolors$dunkelorange  <- darker(zhcolors$orange)
zhcolors$dunkelgelb    <- darker(zhcolors$gelb)
zhcolors$dunkeltuerkis <- darker(zhcolors$tuerkis)
zhcolors$dunkelgruen   <- darker(zhcolors$gruen)
