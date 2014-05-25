# Option Plotter:
# Ploption


require(RQuantLib)
require(ggplot2)
require(scales)

# Implementing Second Leg::
# we need:
#       CP
#       Strike
#       dte
#       premium
#       contracts

ploption <- function(cp, underlying, strike, div, rf, dte, vol, prem, contracts, 
                     cp2='call', strike2=0, dte2=0, prem2=0, contracts2=0, 
                     cp3='call', strike3=0, dte3=0, prem3=0, contracts3=0, 
                     cp4='call', strike4=0, dte4=0, prem4=0, contracts4=0){
    # Set parameters for the x axis :: 25% above and below at-the-money
    xaxis <- seq(underlying*.75, underlying*1.25, underlying*.001)
    
    # Create P&L vectors: Today, Expiry, and halfway 'til expiry
    pnl <- vector()
    pnl2 <- vector()
    pnl3 <- vector()
    
    # Iterate over all stock prices, incremented by the sequence (underlying*.005)
    # Enter 
    for (i in xaxis)
    {        
        # Today!
        pnl <- c(pnl, (AmericanOption(cp, i, strike, div, rf, dte/365, vol)$value*contracts + prem*contracts) + 
                           (AmericanOption(cp2, i, strike2, div, rf, dte2/365, vol)$value*contracts2 - prem2*contracts2) + 
                           (AmericanOption(cp3, i, strike3, div, rf, dte3/365, vol)$value*contracts3 - prem3*contracts3) + 
                           (AmericanOption(cp4, i, strike4, div, rf, dte4/365, vol)$value*contracts4 + prem4*contracts4))
        ## Midway to Expiry
        
        pnl2 <- c(pnl2, (AmericanOption(cp, i, strike, div, rf, dte/730, vol)$value*contracts + prem*contracts) + 
                      (AmericanOption(cp2, i, strike2, div, rf, dte2/730, vol)$value*contracts2 - prem2*contracts2) + 
                      (AmericanOption(cp3, i, strike3, div, rf, dte3/730, vol)$value*contracts3 - prem3*contracts3) + 
                      (AmericanOption(cp4, i, strike4, div, rf, dte4/730, vol)$value*contracts4 + prem4*contracts4))
        
        
        ## AT Expiry
        if(cp=='call')
        {
            payoff=max(i-strike,0)
        }
        if(cp=='put')
        {
            payoff=max(strike-i,0)
        }
        if(cp2=='call')
        {
            payoff2=max(i-strike2,0)
        }
        if(cp2=='put')
        {
            payoff2=max(strike2-i,0)
        }      
        if(cp3=='call')
        {
            payoff3=max(i-strike3,0)
        }
        if(cp3=='put')
        {
            payoff3=max(strike3-i,0)
        }
        if(cp4=='call')
        {
            payoff4=max(i-strike4,0)
        }
        if(cp4=='put')
        {
            payoff4=max(strike4-i,0)
        }
        
        pnl3 <- c(pnl3, payoff*contracts + prem*contracts*sign(contracts) + 
                      payoff2*contracts2 + prem2*contracts2*sign(contracts2) + 
                      payoff3*contracts3 + prem3*contracts3*sign(contracts3) + 
                      payoff4*contracts4 + prem4*contracts4*sign(contracts4))
    }
    
    
    # Profit and Loss Statistics
    print(paste0("Max Profit at Expiry: $", max(pnl3)*100))
    print(paste0("Max Loss at Expiry: $", min(pnl3)*100))
    
    
    qplot(xaxis, pnl3) + geom_line(aes(y=pnl, color=pnl), alpha=.8) +
        geom_line(aes(y=pnl2, color=pnl2), alpha=.9) + 
        geom_point(aes(y=pnl3, color=pnl3)) + 
        scale_colour_gradient2(limits=c(min(pnl3,pnl2,pnl),max(pnl,pnl2,pnl3)), low="red", mid="white", high=muted("green")) + 
        theme(panel.background = element_rect(fill = 'lightblue'))
    
}
#ploption <- function(cp, underlying, strike, div, rf, dte, vol, prem, contracts, 
#cp2, strike2, dte2, prem2, contracts2 
#cp3, strike3, dte3, prem3, contracts3
#cp4, strike4, dte4, prem4, contracts4)
ploption(cp='call', underlying=190, strike=200, div=.014, rf=.005, dte=65, vol=.16, prem=-.43, contracts=1) 
         cp2='call', strike2=195, dte2=65, prem2=1.59, contracts2=-1, 
         cp3='put', strike3=175, dte3=65, prem3=1.15, contracts3=-1, 
         cp4='put', strike4=170, dte4=65, prem4=-.74, contracts4=1)
