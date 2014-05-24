# install_github("plotly", "ropensci", ref = "50f70b8")
library (plotly)

stepsLine <- function (filename = "Export.csv", username="caluchko", key="cqdk1g1p7k",
                       LOWESS = TRUE){
  p <- plotly(username="caluchko", key="cqdk1g1p7k")
  steps <- read.csv(filename, header = TRUE)
  steps$date1 <- as.Date(steps[,1], format = "%m/%d/%y")
  steps [,1] <- format(steps$date1, format = "%Y-%m-%d 00:00:00")
  
  steps <- steps[steps[,2] != 0,]
  steps <- steps[2:length(steps$Steps),]
  
  Steps <- with (steps, list(
    x = Date, y = Steps, name = "Steps"
  ))
  
  fit <- with (steps, lowess(date1, Steps, f = .2)) # note non-default f 
  Lowess <- list(x = steps$Date, y = rev(fit$y), name = "Lowess")
  
  response <- p$plotly(Steps, Lowess, kwargs=list(filename="test2", fileopt="overwrite"))
  
  url <- response$url
  filename <- response$filename
  
  plot(steps$date1, steps$Steps, type = "l")
  lines(fit, col = "red")  
}
