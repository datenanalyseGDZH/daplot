# This example shows how to create a two-page
# document, where each page is a slide with
# title, subtitle, page number and a simple
# plot.
#
# This generates the same output as example 2a,
# but uses separate functions for each page.
# This is better for creating many and complex
# pages.
#
# Please note, that the page numbers still are
# automatically assigned. If you were to switch
# the pages in the final statement, then the
# pages would still be numbered sequentially.

library(extrafont)
library(ggplot2)
library(daplot)

# extrafont::font_import()
loadfonts(device = "win", quiet = TRUE)

create_page1 <- function(doc) {
  doc %>%
    start_page() %>%
    add_slide_title("First page") %>%
    add_slide_subtitle("A simple plot of speed vs distance") %>%
    add_slide_pagenr() %>%
    add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
                 "default", rect = c(10, 28, 150, 110))
}

create_page2 <- function(doc) {
  doc %>%
    start_page() %>%
    add_slide_title("Second page") %>%
    add_slide_subtitle("Something about flowers") %>%
    add_slide_pagenr() %>%
    add_plot_box(ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, col = Species)),
                 "default", rect = c(10, 28, 150, 110))
}

document(title = "Example 2b", format = "slide", theme = get_slide_theme()) %>%
  create_page1() %>%
  create_page2() %>%
  save()
