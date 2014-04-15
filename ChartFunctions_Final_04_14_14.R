BearishCharts <- function(filename, keyRSI=60, nd=40)
{
    ## PRECONDITION:: stockList$Ticker is a list of stock Tickers;
    ##                quantmod is loaded
    
    ## POSTCONDITION: Returns charts of stocks that have the following bearish traits:
    #                   (1) Near the upper bollinger band (pctB > .9)
    #                   (2) Higher volatility (ATR >= mean)
    stockList <- read.csv(filename, header=TRUE)
    BearTickers <- vector()
    library(quantmod)
    Custom.TA <- paste("addBBands(n=", nd, "); addATR(n=", nd, "); addRSI(n=", nd,");",sep="")
    for (i in seq_along(stockList$Ticker))
    {   
        try(x <- get(paste(stockList$Ticker[i]))) # store quant data in x
        bbands.HLC <- BBands(x[,2:4])        # calculate bband info

        # if it is at the upper portion of bands.. 
        if(tail(bbands.HLC$pctB,n=1) >= 0.9)
        {
            xATR <- ATR(x[,2:4])
            xATR <- xATR['2012-01-01::']
                   
            if(tail(xATR$atr,n=1) > mean(xATR$atr,na.rm=TRUE))
            {
                candleChart(last(get(paste(stockList$Ticker[i])), '9 months'),
                            multi.col=TRUE,theme='white', TA=Custom.TA, 
                            name=as.character(stockList$Ticker[i]))
                BearTickers <- c(BearTickers, paste(stockList$Company[i], " (", stockList$Ticker[i], ")", sep=""))
            }
        }
    }
    
    print(paste(length(BearTickers), "tickers show bearish signals:"))
    print(BearTickers)
}



BullishCharts <- function(filename, keyRSI=40, nd=40){
    ## PRECONDITION:: filename is name of csv
    ##                quantmod is loaded
    
    ## POSTCONDITION: Returns charts of stocks that have the following bullish traits:
    #                   (1) Near the lower bollinger band (pctB < .1)
    #                   (2) Higher volatility (ATR >= mean)
    
    stockList <- read.csv(filename, header=TRUE)
    myTA <- paste("addBBands(n=40); addATR(n=40); addRSI(n=20);",sep="")
    
    
    library(quantmod)
    BullTickers <- vector()
    for (i in seq_along(stockList$Ticker))
    {   
        try(x <- get(paste(stockList$Ticker[i])), silent=TRUE) # store quant data in x
        bbands.HLC <- BBands(x[,2:4])        # calculate bband info
        
        # if it is at lower portion of bands.. 
        if(tail(bbands.HLC$pctB,n=1) <= 0.1)
        {
            xATR <- ATR(x[,2:4],n=nd)
            xATR <- xATR['2012-01-01::']
            tail(xATR)
            if(tail(xATR$atr,n=1) > mean(xATR$atr,na.rm=TRUE))
            {
                xRSI <- RSI(x[,4])
                if(tail(xRSI,n=1) < 30)
                {
                    candleChart(last(get(paste(stockList$Ticker[i])), '9 months'),
                            multi.col=TRUE, theme='white', TA=myTA, 
                            name=as.character(stockList$Ticker[i]))
                    BullTickers <- c(BullTickers, paste(stockList$Company[i], "(", stockList$Ticker[i], ")"))
                }
            }            
        }
    }
    print(paste(length(BullTickers), "tickers show bullish signals:"))
    BullTickers
}

