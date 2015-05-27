library(zoo)
library(PerformanceAnalytics)
nbimcols <- c("black", "#5381AC", "#0098CD", "#FD4239", "#5F6A72", "#002878", "#AA9D71", "#4C6600")


returnindices <- read.csv("hedging.csv")

returnindices <- data.frame(returnindices, stringsAsFactors = FALSE)

returnindices$Date <- as.Date(as.character(returnindices$Date), "%d/%m/%Y")

returnindices <- as.xts(returnindices[2:ncol(returnindices)], order.by = returnindices$Date)

returns <- Return.calculate(returnindices, method ="log")

portfolioreturns <- function(equity_share, hedge_ratio) {
        
        equity_share * ( hedge_ratio * returns$MSCI_H + (1-hedge_ratio) * returns$MSCI_UH) + 
                (1-equity_share) * (hedge_ratio * returns$BGAG_H + (1-hedge_ratio) * returns$BGAG_UH)
        
        
}


# Loop through different equity shares and hedge ratios
# both ranging from 0 to 100% in steps of 10 %

equity_shares <- seq(from = 0, to = 1, by = 0.1)
hedge_ratios <- seq(from = 0, to = 1, by = 0.1)

# both 'risk' and 'reward' matrices have
# equity shares along the rows
# hedge ratios along the columns

risk <- matrix(data=NA, 
               nrow = length(equity_shares), 
               ncol = length(hedge_ratios), 
               dimnames = list(equity_shares, hedge_ratios))

reward <- matrix(data=NA, 
               nrow = length(equity_shares), 
               ncol = length(hedge_ratios),
               dimnames = list(equity_shares, hedge_ratios))

# For every equity-share/hedge-ratio combination 
# we record the portfolio's mean return and standard deviation
# and write them to the matrices 'reward' and 'risk'

k <- 1
l <- 1

for (i in equity_shares) {
        
        
        for (j in hedge_ratios) {
                
                 
                hedge_return <- portfolioreturns(i,j)        
                
                reward[k,l] <- round(mean(hedge_return, 
                                     na.rm = TRUE)*1200, 4)
                   
                risk[k,l] <- round(sd(hedge_return, 
                             na.rm = TRUE)*sqrt(12)*100, 4)
                
                l <- l + 1
                
               
                
                
        }
        
        title <- paste("Equity ratio", as.character(round(i*100,0)), "%", sep=" ")
        
        
        plot(x = reward[k,], y = risk [k,], 
             main = title, 
             xlab = "Return %", 
             ylab = "Risk %",
             type = "l")
        
        k <- k + 1
        l <- 1
      
                
}





