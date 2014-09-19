# install_github("plotly", "ropensci", ref = "50f70b8")
library (plotly)

stepsLine <- function (filename = "export.csv", username="caluchko", key="cqdk1g1p7k",
                       LOWESS = TRUE){
  p <- plotly(username="caluchko", key="cqdk1g1p7k")
  steps <- read.csv(filename, header = TRUE)
  steps$date1 <- as.Date(steps[,1], format = "%m/%d/%Y") # column of date values as date class
  steps [,1] <- format(steps$date1, format = "%Y-%m-%d 00:00:00") # additional column of date values as character vector format readable to plot.ly API
  
  steps <- steps[steps[,2] != 0,] # removes all values of steps that equal zero
  steps <- steps[2:length(steps$Steps),] # removes most recent day's entry
  
  Steps <- with (steps, list(x = Date, y = Steps, name = "Steps")) # list of x and y values of steps
  
  fit <- with (steps, lowess(date1, Steps, f = .2)) # LOWESS fit, note non-default f 
  Lowess <- list(x = steps$Date, y = rev(fit$y), name = "Lowess") # list of x and y values for fit
  
  response <- p$plotly(Steps, Lowess, kwargs=list(filename="test2", fileopt="overwrite")) #plot.ly API function
  
  url <- response$url # URL of new plot on plot.ly
  filename <- response$filename
  
  plot(steps$date1, steps$Steps, type = "l")
  lines(fit, col = "red")  
}
