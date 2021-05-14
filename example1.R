library(extrafont)
library(ggplot2)
library(daplot)

# extrafont::font_import()
loadfonts(device = "win", quiet = TRUE)

plot <- ggplot(data = cars) + geom_point(aes(x = speed, y = dist))

static_plot(format = "folie", theme = get_powerpoint_theme()) %>%
  add_folien_titel("Example1") %>%
  add_folien_subtitel("A simple plot of speed vs distance") %>%
  add_plot_box(ggplot(data = cars) + geom_point(aes(x = speed, y = dist)),
               "default", rect = c(10, 28, 150, 110)) %>%
  save_as("example1")
