getSymbols(c('QQQ','TLT'))


SMA.Strategy.Profit.Or.Loss.ALT(QQQ, TLT, 10000, 1.35, .97, "QQQ & TLT - Profit Target: 35% ; Stop Loss: -3%")

SMA.Strategy.Profit.Or.Loss.ALT <- function(SPY, ALT, portfolio, profit.target, loss.target, title)
{
    # Load in required packages
    require(PerformanceAnalytics)
    require(quantmod)
    
    # Portfolio Balance and necessary TA for signals
    SPY$portfolio <- portfolio
    
    SPY$n20 <- SMA(SPY[,6], n=20)
    SPY$n50 <- SMA(SPY[,6], n=50)
    SPY$n200 <- SMA(SPY[,6], n=200)
    
    # Create Signal
    signal <- rep(FALSE,200)
    for(i in 201:length(SPY[,6]))
    {    
        if(     (SPY[i,6] > SPY$n20[i]) && 
                    (SPY[i,6] < SPY$n50[i]) && 
                    (SPY[i,6] < SPY$n200[i])    )
        {
            signal <- c(signal, TRUE)}
        else
        {
            signal <- c(signal, FALSE)}
    }
    
    # Pre-Trade requirements
    SPY.Returns <- as.vector(as.numeric(dailyReturn(SPY[,6])))
    ALT.Returns <- as.vector(as.numeric(dailyReturn(ALT[,6])))
    SPY$Signal <- signal
    startpoint <- vector()
    startdates <- vector()
    enddates <- vector()
    endpoint <- vector()
    SPY$in.trade <- FALSE
    
    
    for(i in 201:(length(SPY$portfolio)))
    {
        # If we have hit a signal, and we do not already have a position on
        if(signal[i] == TRUE & SPY$in.trade[i] == FALSE)
        { 
            # Used for summarizing statistics, if needed  
            startpoint <- c(startpoint, SPY[i,6])
            startdates <- c(startdates, index(SPY[i,6]))
            
            ## FIND EXIT POINT FROM PROFIT/LOSS
            for(x in i:(length(SPY$portfolio)))
            {
                if((as.numeric(SPY[x,6])/as.numeric(SPY[i,6]) >= profit.target) | (as.numeric(SPY[x,6])/as.numeric(SPY[i,6]) <= loss.target))
                {
                    endpoint <- c(endpoint, SPY[x,6])
                    enddates <- c(enddates, index(SPY[x,6]))
                    y <- x
                    break
                }
            }
            
            SPY$in.trade[i:y] <- TRUE
            
            #start.date <- index(SPY$portfolio[i])
            #end.date <- index(SPY$portfolio[y])
            #lineChart(SPY[paste0(start.date,"::",end.date)], TA="addTA(SPY$n20, on=1);addTA(SPY$n50, on=1);addTA(SPY$n200,on=1)")
            
        }   
        
        # Adjust the daily P&L every day in we're in trade
        
        if(SPY$in.trade[i] == TRUE)
        {
            
            SPY$portfolio[i] <- SPY$portfolio[i-1]*(1+SPY.Returns[i])
        }
        
        # No Position on.
        if(SPY$in.trade[i] == FALSE)
        {
            SPY$portfolio[i] <- SPY$portfolio[i-1]*(1+ALT.Returns[i])   }
        
        
        
    }
    SPY$portfolio[(length(SPY$portfolio)-holding.period):(length(SPY$portfolio))] <- SPY$portfolio[(length(SPY$portfolio)-holding.period)]
    comp <- data.frame(strategy.A = dailyReturn(SPY$portfolio), buy.and.hold = dailyReturn(SPY[,6]), alternative=dailyReturn(ALT[,6]))
    charts.PerformanceSummary(comp, main=title)
    chartSeries(SPY$portfolio, type='l', name=title)
    print(paste0("Sharpe Ratio for Strategy: ", AdjustedSharpeRatio(comp[,1,drop=FALSE])))
    print(paste0("Sharpe Ratio for Buy-and-Hold: ", AdjustedSharpeRatio(comp[,2,drop=FALSE])))
    
}

