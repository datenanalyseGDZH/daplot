# This example shows how to create a two-page
# document, where each page is a slide with
# title, subtitle, page number and a simple
# plot.
#
# This variant shows how to generate everything
# in a single pipeline. If the pages are more
# complex, it might not be feasible to do it
# all in one swoop. For a better approach see
# example 2b.

library(extrafont)
library(ggplot2)
library(daplot)

# extrafont::font_import()
loadfonts(device = "win", quiet = TRUE)

document(title = "Example 2a", format = "slide", theme = get_slide_theme()) %>%
  start_page() %>%
  add_slide_title("First page") %>%
  add_slide_subtitle("A simple plot of speed vs distance") %>%
  add_slide_pagenr() %>%
  add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
               "default", rect = c(10, 28, 150, 110)) %>%
  start_page() %>%
  add_slide_title("Second page") %>%
  add_slide_subtitle("Something about flowers") %>%
  add_slide_pagenr() %>%
  add_plot_box(ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, col = Species)),
               "default", rect = c(10, 28, 150, 110)) %>%
  save()
