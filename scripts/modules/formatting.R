add_dark_theme <- function(input_ggplot) {

    input_ggplot +  theme(
        text = element_text(
            size = 20,
            color="#adadad"),
        plot.background = element_rect(
            fill = "#212121",
            colour = "#212121"),
        panel.background = element_rect(
            fill = "#303030",
            colour = "#303030",
            size = 0.5,
            linetype = "solid"),
        panel.grid.major = element_line(
            size = 0.5, linetype = 'solid',
            colour = "#adadad"), 
        panel.grid.minor = element_line(
            size = 0.25,
            linetype = 'solid',
            colour = "#adadad"),
        axis.text = element_text(
            color="#adadad"),
        legend.background = element_rect(
            fill = "#212121", 
            colour = "transparent")) %>%
        return()
}