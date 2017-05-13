library(ggplot2)
library(ggthemes)
library(scales)

cap4theme <- function(minor=F) {
  theme_foundation(base_size = 12, 
                   base_family = "Times") +
    theme(line = element_line(colour = "#2c3e50"),
          rect = element_rect(fill = "#ffffff", linetype = 0, colour = NA),
          text = element_text(colour = "#2c3e50"),
          axis.text = element_text(),   axis.ticks = element_blank(),
          axis.line = element_blank(),  legend.background = element_rect(),
          legend.position = "bottom",   legend.direction = "horizontal",
          legend.box      = "vertical", panel.grid = element_line(colour = NULL),
          panel.grid.major = element_line(colour = "#bdc3c7"),
          panel.grid.minor = if (minor==T) {element_line(colour = "#ecf0f1")} else {element_blank()},
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          strip.background = element_rect())
}


# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_line() + ggtitle('hello')+cap4theme()
# ggsave('../../cleandata/test.pdf', width=11, height=8.5, units="in")
