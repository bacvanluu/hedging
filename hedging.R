library(zoo)
library(PerformanceAnalytics)
nbimcols <- c("black", "#5381AC", "#0098CD", "#FD4239", "#5F6A72", "#002878", "#AA9D71", "#4C6600")


returnindices <- read.csv("hedging.csv")

returnindices <- data.frame(returnindices, stringsAsFactors = FALSE)

returnindices$Date <- as.Date(as.character(returnindices$Date), "%d/%m/%Y")

returnindices <- as.xts(returnindices[2:ncol(returns)], order.by = returnindices$Date)

returns <- Return.calculate(returnseries, method ="log")

portfolioreturns <- function(equity_share, hedge_ratio) {
        
        hedge_ratio * (equity_share * returns$MSCI_H + (1-equity_share) *returns$BGAG_H)
                                + (1-hedge_ratio) * (equity_share * returns$MSCI_UH + (1-equity_share) * returns$BGAG_UH)
        
        
}


# Loop through different equity shares and hedge ratios
# both ranging from 0 to 100% in steps of 5 %

equity_shares <- seq(from = 0, to = 1, by = 0.05)
hedge_ratios <- seq(from = 0, to = 1, by = 0.05)
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
                                     na.rm = TRUE)*100, 2)
                   
                risk[k,l] <- round(sd(hedge_return, 
                             na.rm = TRUE)*100, 2)
                
                l <- l + 1
                
                
        }
        
        k <- k + 1
        l <- 1
      
                
}



