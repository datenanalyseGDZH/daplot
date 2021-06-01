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

library(ggplot2)
library(daplot)

fill_page1 <- function(doc) {
  doc %>%
    set_slide_title("First page") %>%
    set_slide_subtitle("A simple plot of speed vs distance") %>%
    set_slide_pagenr() %>%
    add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
                 "default", rect = c(10, 28, 150, 110))
}

fill_page2 <- function(doc) {
  doc %>%
    set_slide_title("Second page") %>%
    set_slide_subtitle("Something about flowers") %>%
    set_slide_pagenr() %>%
    add_plot_box(ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, col = Species)),
                 "default", rect = c(10, 28, 150, 110))
}

fill_page3 <- function(doc) {
  doc %>%
    set_slide_title("Third page") %>%
    set_slide_subtitle("A Barchart") %>%
    set_slide_pagenr() %>%
    add_plot_box(ggplot(data = df) +
                   geom_bar(aes(x = n, y = value), width = 0.8,
                            col = "blue", fill = "blue", stat = "identity"),
                 "default", rect = c(10, 28, 150, 110))
}

create_document(title = "Example 2b", format = "slide_4x3", theme = get_slide_4x3_theme()) %>%
  start_page() %>% fill_page1() %>%
  start_page() %>% fill_page2() %>%
  start_page() %>% fill_page3() %>%
  save_pdf()
