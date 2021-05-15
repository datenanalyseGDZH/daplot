# This example shows how to generate a simple slide containing
# title, subtitle and a simple plot.

library(ggplot2)
library(daplot)

# Example saved as PNG

page(format = "slide", theme = get_slide_theme()) %>%
  add_slide_title("Example1") %>%
  add_slide_subtitle("A simple plot of speed vs distance") %>%
  add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
               "default", rect = c(10, 28, 150, 110)) %>%
  save_png("example1")


# Example saved as PDF

page(format = "slide", theme = get_slide_theme()) %>%
  add_slide_title("Example1") %>%
  add_slide_subtitle("A simple plot of speed vs distance") %>%
  add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
               "default", rect = c(10, 28, 150, 110)) %>%
  save_pdf("example1")
