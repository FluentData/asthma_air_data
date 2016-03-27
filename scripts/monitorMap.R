monitorMap <- function(res) {

  all <- res$data[, c("Longitude", "Latitude", "First.Year.of.Data", "Last.Year.of.Data")]
  active <- all[all$Last.Year.of.Data == 2015, ]
  complete <- active[active$First.Year.of.Data <= 1999, ]

  m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2, byrow = TRUE)

  #par(oma = c(0, 0, 2, 0))

  layout(mat = m,heights = c(0.8,0.2))

  map("county", "indiana")
  points(all$Longitude, all$Latitude, pch=19, col = "pink")
  points(active$Longitude, active$Latitude, pch=19, col = "lightgreen")
  points(complete$Longitude, complete$Latitude, pch=19, col = "violet")
  title("Statewide", cex = 0.3)

  map("county", "indiana,marion")
  points(all$Longitude, all$Latitude, pch=19, col = "pink")
  points(active$Longitude, active$Latitude, pch=19, col = "lightgreen")
  points(complete$Longitude, complete$Latitude, pch=19, col = "violet")
  title("Marion County")

  par(mar=c(1,1,1,1))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  plot_colors <- c("blue","black", "green", "orange", "pink")
  legend("center", # position
         legend = c("Complete Data", "Currently Active", "Other Monitors"),
         xjust = 1,
         yjust = 0,
         col = c("violet", "lightgreen", "pink"),
         pch = 19,
         horiz = TRUE,
         bty = "n") # border
  mtext(paste(simpleCap(res$name), "Monitoring Locations"), outer = TRUE, cex = 1.5)

}
