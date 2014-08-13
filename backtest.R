library(RCurl)
library(quantmod)
library(PerformanceAnalytics)
x <- getURL('https://raw.githubusercontent.com/MichaelJulian/finance_in_r/master/XIV_Signal.txt')
signal <- read.csv(text=x)

# get XIV as xts data
getSymbols('XIV')
# Split Adjust, adjust range for the available signal
XIV <- XIV["2011-05-11::2014-06-02"]
XIV$XIV.Open["::2011-06-24"] <- XIV$XIV.Open["::2011-06-24"]/10
XIV$XIV.High["::2011-06-24"] <- XIV$XIV.High["::2011-06-24"]/10
XIV$XIV.Low["::2011-06-24"] <- XIV$XIV.Low["::2011-06-24"]/10
XIV$XIV.Close["::2011-06-24"] <- XIV$XIV.Close["::2011-06-24"]/10
XIV$Signal <- signal$Signal


startpoint <- vector()
endpoint <- vector()
XIV$in.trade <- FALSE
XIV$portfolio <- 20000
holding.period = 45
XIV.Returns <- as.vector(dailyReturn(XIV)["2011-05-11::2014-06-02"])


for(i in 2:(length(XIV$portfolio)-holding.period))
{
    # If we have hit a signal, and we do not already have a position on
    if(XIV$Signal[i] == TRUE & XIV$in.trade[i] == FALSE)
    { 
        # Set # Shares equal to the amount of cash we have at EOD yesterday
        xiv.shares <- as.numeric((XIV$portfolio[(i-1)])/XIV$XIV.Close[i])
        x <- i
        
        # "in.trade" will be TRUE for the next (n=holding.period) days
        XIV$in.trade[i:(i+holding.period)] <- TRUE
        
        # Used for keeping track of avg P/L from signal
        startpoint <- c(startpoint, XIV$XIV.Close[i])
        endpoint <- c(endpoint, XIV$XIV.Close[(i+holding.period)])
        
    }
    
    # Adjust the daily P&L every day in we're in trade
    if(XIV$in.trade[i] == TRUE)
    {
        
        XIV$portfolio[i] <- XIV$portfolio[i-1]*(1+XIV.Returns[i])
    }
    
    # No Position on.
    if(XIV$in.trade[i] == FALSE)
    {
        XIV$portfolio[i] <- XIV$portfolio[i-1]
    }
    
    
    
    
}
# Adjust the last days that weren't included in the 'for' loop
XIV$portfolio[725:769] <- XIV$portfolio[724]
plot(XIV$portfolio, type='l')

stats <- cbind(startpoint, endpoint, change=(endpoint-startpoint)/startpoint)
summary(stats)


comparison <- dailyReturn(XIV)
comparison$strategy.returns <- dailyReturn(XIV$portfolio)

charts.PerformanceSummary(comparison[,c(1,2)])
