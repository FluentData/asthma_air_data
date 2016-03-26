library(ggplot2)
library(reshape2)

load("./data/full.data.rda")

data <- full.data

data$Last.Sample.Date <- as.Date(data$Last.Sample.Date, "%Y-%m-%d")
data$Last.Year.of.Data <- as.numeric(format(data$Last.Sample.Date, "%Y"))
data$First.Year.of.Data <- as.numeric(data$First.Year.of.Data)
data$Site.Code <- sprintf("%02s-%03i-%04i-%i", data$State.Code, data$County.Code,
                          data$Site.Number, data$POC)


data <- data[!is.na(data$Last.Sample.Date), ]
data <- data[data$Last.Sample.Date > as.Date("1999-01-01"), ]
data <- data[data$State.Code == 18, ]

ozone <- data[data$Parameter.Name == "Ozone", ]

o3plot_data <- ozone[, c("Site.Code", "First.Year.of.Data", "Last.Year.of.Data")]
o3plot_data$Last.Year.of.Data <- o3plot_data$Last.Year.of.Data - o3plot_data$First.Year.of.Data
o3plot_data <- melt(o3plot_data, id.vars = "Site.Code")
o3plot_data$value <- as.numeric(as.character(o3plot_data$value))

ozone_plot <- ggplot(o3plot_data, aes(x = Site.Code, y = value, fill = variable)) +
  coord_flip(ylim = c(1999, 2015)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c(alpha("blue", 0), "blue"))
plot(ozone_plot)
