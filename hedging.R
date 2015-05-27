library(zoo)
library(PerformanceAnalytics)
nbimcols <- c("black", "#5381AC", "#0098CD", "#FD4239", "#5F6A72", "#002878", "#AA9D71", "#4C6600")


returnindices <- read.csv("hedging.csv")

returns <- data.frame(returnindices, stringsAsFactors = FALSE)

returns$Date <- as.Date(as.character(returns$Date), "%d/%m/%Y")

returnseries <- as.xts(returns[2:ncol(returns)], order.by = returns$Date)

Return.calculate(returnseries, method ="log")

