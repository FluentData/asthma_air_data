coveragePlot <- function(data, main) {

  plot_data <- data %>%
    select(Site.Code, First.Year.of.Data, Last.Year.of.Data) %>%
    mutate(Last.Year.of.Data = Last.Year.of.Data - First.Year.of.Data) %>%
    melt(id.vars = "Site.Code") %>%
    mutate(value = as.numeric(as.character(value)))

  pol_plot <- ggplot(plot_data, aes(x = Site.Code, y = value, fill = variable)) +
    geom_bar(stat = "identity") + coord_flip(ylim = c(1999, 2015)) +
    scale_fill_manual(values = c(alpha("blue", 0), "blue")) +
    theme(legend.position="none") + ggtitle(main) + ylab("Year") +
    xlab("Monitor ID")

  plot(pol_plot)

}
