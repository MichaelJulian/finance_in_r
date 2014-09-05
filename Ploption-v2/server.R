library(quantmod)
library(ggplot2)
library(shiny)
library(scales)
library(devtools)
library(rCharts)
library(fOptions)
require(ggplot2)

bsm.value <- function(spot, strike, vol, r, div, t, q, flag)
{
    if(flag=='call')
    {
        option.price <- call.option(spot, strike, vol, r, div, t, q)
    }
    else
    {
        option.price <- put.option(spot, strike, vol, r, div, t, q)
    }
    
    option.price
}

call.option <- function(spot, strike, vol, r, div, t, q)
{
    
    s0eqt <- spot*exp(-q*t)
    
    d1 <- f.d1(spot, strike, vol, r, div, t)
    n.d1 <- pnorm(d1)
    
    
    Xert <- strike*exp(-r*t)
    d2 <- f.d2(d1, vol, t)
    n.d2 <- pnorm(d2)
    
    s0eqt*n.d1 - Xert*n.d2
    
}

put.option <- function(spot, strike, vol, r, div, t, q)
{
    Xert <- strike*exp(-r*t)
    d1 <- f.d1(spot, strike, vol, r, div, t)
    d2 <- f.d2(d1, vol, t)
    n.d2 <- pnorm(-d2)
    
    s0eqt <- spot*exp(-q*t)
    n.d1 <- pnorm(-d1)
    
    Xert*n.d2 - s0eqt*n.d1
}

net.greeks <- function(spot, strike, vol, r, div, t, q, flag, contracts)
{
    if(flag=='call')
    {
        ### Calculate Call Option Price
        
        eqt <- exp(-q*t)
        s0eqt <- spot*eqt
        d1 <- f.d1(spot, strike, vol, r, div, t)
        n.d1 <- pnorm(d1)
        
        ert <- exp(-r*t)
        Xert <- strike*ert
        d2 <- f.d2(d1, vol, t)
        n.d2 <- pnorm(d2)
        
        option.price = contracts*round(s0eqt*n.d1 - Xert*n.d2, digits=6)
        ### Calculate Call Delta
        option.delta <- contracts*round(exp(-q*t)*n.d1, digits=6)
        ### Calculate Call Gamma
        g1 <- eqt/(spot*vol*t^0.5)
        g2 <- (2*pi)^-.5
        g3 <- exp(-d1^2/2)
        option.gamma <- contracts*round(g1*g2*g3, digits=6)
        
        ### Calculate Call Theta
        t1.a <- (spot*vol*eqt)/(2*t^0.5)
        t1 <- -(t1.a*g2*g3)    
        t2 <- r*strike*ert*n.d2
        t3 <- q*spot*eqt*n.d1
        TT <- 1/365
        
        option.theta <- contracts*round(TT*(t1-t2+t3), digits=6)
        
        ### Calculate Call Vega
        v1 <- (1/100)*spot*eqt*t^0.5
        option.vega <- contracts*round(v1*g2*g3, digits=6)
        
        
    }
    if(flag=='put')
    {
        ert <- exp(-r*t)
        Xert <- strike*exp(-r*t)
        d1 <- f.d1(spot, strike, vol, r, div, t)
        d2 <- f.d2(d1, vol, t)
        n.d2 <- pnorm(-d2)
        
        eqt <- exp(-q*t)
        s0eqt <- spot*exp(-q*t)
        n.d1 <- pnorm(-d1)
        n.pd1 <- pnorm(d1)
        
        
        ### Calculate Put Option Price
        option.price = contracts*round(Xert*n.d2 - s0eqt*n.d1, digits=6)
        
        ### Calculate Put Delta
        option.delta = contracts*round(exp(-q*t)*(n.pd1-1), digits=6)
        
        ### Calculate Put Gamma
        g1 <- eqt/(spot*vol*t^0.5)
        g2 <- (2*pi)^-.5
        g3 <- exp(-d1^2/2)
        
        option.gamma <- contracts*round(g1*g2*g3, digits=6)
        
        ### Calculate Put Theta
        t1.a <- (spot*vol*eqt)/(2*t^0.5)
        t1 <- -(t1.a*g2*g3)    
        t2 <- r*strike*ert*n.d2
        t3 <- q*spot*eqt*n.d1
        TT <- 1/365
        option.theta <- contracts*round(TT*(t1+t2-t3),digits=6)
        
        ### Calculate Put Vega
        v1 <- (1/100)*spot*eqt*t^0.5
        option.vega <- contracts*round(v1*g2*g3, digits=6)
        
    }
    
    data.frame(Type=flag, Strike=strike, Vol=vol, Delta=option.delta*100,
               Gamma=option.gamma*100, Theta=option.theta*100, Vega=option.vega*100, Contracts=contracts)
    
}
f.d1 <- function(spot, strike, vol, r, div, t)
{
    (log(spot/strike) + t*(r+(vol^2)/2))/(vol*t^0.5)
}

f.d2 <- function(d1, vol, t)
{
    d1 - vol*(t^0.5)
}

shinyServer(function(input, output) {
    
    output$pnlChart_gg <- renderPlot({
        stockprice <- seq(from=input$spot*.9, to=input$spot*1.1, by=.1)
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365
        
        vol.1 <- input$vol.1/100
        vol.2 <- input$vol.2/100
        
        shares.pnl <- (stockprice - input$share.price)*input$shares
        
        leg1.today <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, t, input$rf, input$leg1.flag) - input$leg1.cost 
        leg1.between <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, t/2, input$rf, input$leg1.flag) - input$leg1.cost 
        leg1.expiry <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, 0, input$rf, input$leg1.flag) - input$leg1.cost 
        #         leg1.greeks.today <- net.greeks(input$spot, input$leg1.strike, vol, input$rf, input$div, t, input$rf, input$leg1.flag, input$leg1.contracts)
        
        leg2.today <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, t, input$rf, input$leg2.flag) - input$leg2.cost 
        leg2.between <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, t/2, input$rf, input$leg2.flag) - input$leg2.cost 
        leg2.expiry <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, 0, input$rf, input$leg2.flag) - input$leg2.cost 
        #        leg2.greeks.today <- net.greeks(input$spot, input$leg2.strike, vol, input$rf, input$div, t, input$rf, input$leg2.flag, input$leg2.contracts)
        
        spread.today <- round(input$leg1.contracts*leg1.today + input$leg2.contracts*leg2.today, digits=2)*100  + shares.pnl
        spread.between <- round(input$leg1.contracts*leg1.between + input$leg2.contracts*leg2.between, digits=2)*100 + shares.pnl
        spread.expiry <- round(input$leg1.contracts*leg1.expiry + input$leg2.contracts*leg2.expiry, digits=2)*100 + shares.pnl
        dataz <- data.frame(stockprice, spread.today, spread.between, spread.expiry)
        
        myPlot <- ggplot(dataz, aes(stockprice, spread.expiry), xlab="Underlying Stock Price", ylab="P/L") + geom_area(data=subset(dataz, spread.expiry <=0), fill="red", alpha=0.2) + geom_area(data=subset(dataz, spread.expiry >= 0), fill="green", alpha=0.2)
        myPlot <- myPlot + geom_point(aes(y=spread.expiry, color=spread.expiry), alpha=.8, data=dataz)# + geom_area(data=subset(dataz, spread.today <=0), fill='red', alpha=0.2) + geom_area(data=subset(dataz, spread.today >=0), fill='green', alpha=0.2)
        myPlot <- myPlot + scale_colour_gradient2(limits=c(min(spread.today,spread.between,spread.expiry),max(spread.today,spread.between,spread.expiry)), low="red", mid="white", high=muted("green"))
        #myPlot <- myPlot + geom_ribbon(data=dataz, aes(ymin=0, ymax=spread.expiry), alpha=0.2, fill = 'green')
        #myPlot <- myPlot + geom_ribbon(aes(ymin=spread.expiry, ymax=0), alpha=0.2, fill = 'red', data=dataz)
        myPlot <- myPlot + geom_point(aes(y=spread.today, color=spread.today), alpha=.8, data=dataz)
        myPlot <- myPlot + geom_line(aes(y=spread.between, color=spread.between), alpha=.9, data=dataz)
        #myPlot <- myPlot + geom_point(aes(y=spread.today, color=spread.today), data=dataz)
        #myplot <- myPlot + geom_line( aes(stockprice, spread.expiry), data=dataz) + geom_area(data=subset(dataz, spread.expiry <=0), fill="red", alpha=0.2) + geom_area(data=subset(dataz, spread.expiry >= 0), fill="green", alpha=0.2)
        #myPlot <- myPlot + geom_ribbon( aes(ymax=0, ymin=spread.expiry), alpha=0.2, fill = 'black', data=dataz)
        
        myPlot <- myPlot + theme(panel.background = element_rect(fill = 'lightblue')) + geom_hline(aes(y=0), alpha=.4)
#             
        myPlot
        
        })
    
    output$pnlChart <- renderChart({
        
        
        stockprice <- seq(from=input$spot*.75, to=input$spot*1.25, by=.1)
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365
        vol.1 <- input$vol.1/100
        vol <- input$vol.2/100
        vol.2 <- input$vol.2/100
        
        leg1.today <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, t, input$rf, input$leg1.flag) - input$leg1.cost
        leg1.between <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, t/2, input$rf, input$leg1.flag) - input$leg1.cost
        leg1.expiry <- bsm.value(stockprice, input$leg1.strike, vol.1, input$rf, input$div, 0, input$rf, input$leg1.flag) - input$leg1.cost
#         leg1.greeks.today <- net.greeks(input$spot, input$leg1.strike, vol, input$rf, input$div, t, input$rf, input$leg1.flag, input$leg1.contracts)
        
        leg2.today <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, t, input$rf, input$leg2.flag) - input$leg2.cost
        leg2.between <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, t/2, input$rf, input$leg2.flag) - input$leg2.cost
        leg2.expiry <- bsm.value(stockprice, input$leg2.strike, vol.2, input$rf, input$div, 0, input$rf, input$leg2.flag) - input$leg2.cost
#        leg2.greeks.today <- net.greeks(input$spot, input$leg2.strike, vol, input$rf, input$div, t, input$rf, input$leg2.flag, input$leg2.contracts)
        
        spread.today <- round(input$leg1.contracts*leg1.today + input$leg2.contracts*leg2.today, digits=2)
        dataf <- data.frame(spread.today, stockprice)
        h1 <- hPlot(x='stockprice', y='spread.today', xlab="Stock Price", type ='line', data=dataf)
        h1$addParams(dom = 'pnlChart')
        return(h1)

        
        
    })
    
    
    
    output$leg1.iv_estimate <- renderText({
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365     
        paste0("Est. Implied Volatility: ",as.character(
            round(GBSVolatility(input$leg1.cost, substr(input$leg1.flag,1,1), input$spot, input$leg1.strike, t, input$rf, input$rf, .00001, 100000)*100, digits=2)), "%")       
    })
    output$leg2.iv_estimate <- renderText({
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365     
        paste0("Est. Leg 2 % IV: ",as.character(
            round(GBSVolatility(input$leg2.cost, substr(input$leg2.flag,1,1), input$spot, input$leg2.strike, t, input$rf, input$rf, .0001, 10000)*100, digits=2)), "%")       
    })


    output$leg_greeks <- renderTable({
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365
        leg1.vol <- GBSVolatility(input$leg1.cost, substr(input$leg1.flag,1,1), input$spot, input$leg1.strike, t, input$rf, input$rf, .01, 100)
        leg1.greeks.today <- net.greeks(input$spot, input$leg1.strike, leg1.vol, input$rf, input$div, t, input$rf, input$leg1.flag, input$leg1.contracts)
        leg2.vol <- GBSVolatility(input$leg2.cost, substr(input$leg2.flag,1,1), input$spot, input$leg2.strike, t, input$rf, input$rf, .01, 100)
        leg2.greeks.today <- net.greeks(input$spot, input$leg2.strike, leg2.vol, input$rf, input$div, t, input$rf, input$leg2.flag, input$leg2.contracts)
        shares.today <- data.frame(Type='shares', Strike=input$share.price, Vol=0, Delta=input$shares, Gamma=0, Theta=0, Vega=0, Contracts=0)
        spread.greeks.today <- rbind(leg1.greeks.today, leg2.greeks.today, shares.today)
        spread.net.stats <- data.frame(Cost=sum(spread.greeks.today$Price),Net.Delta=sum(spread.greeks.today$Delta), Net.Gamma=sum(spread.greeks.today$Gamma), Net.Theta=sum(spread.greeks.today$Theta), Net.Vega=sum(spread.greeks.today$Vega))
        spread.greeks.today
    })
    output$net_greeks <- renderTable({
        
        t <- (as.numeric(input$dates[2])-as.numeric(input$dates[1]))/365
        leg1.vol <- GBSVolatility(input$leg1.cost, substr(input$leg1.flag,1,1), input$spot, input$leg1.strike, t, input$rf, input$rf, .01, 100)
        leg1.greeks.today <- net.greeks(input$spot, input$leg1.strike, leg1.vol, input$rf, input$div, t, input$rf, input$leg1.flag, input$leg1.contracts)
        leg2.vol <- GBSVolatility(input$leg2.cost, substr(input$leg2.flag,1,1), input$spot, input$leg2.strike, t, input$rf, input$rf, .01, 100)
        leg2.greeks.today <- net.greeks(input$spot, input$leg2.strike, leg2.vol, input$rf, input$div, t, input$rf, input$leg2.flag, input$leg2.contracts)
        
        spread.greeks.today <- rbind(leg1.greeks.today, leg2.greeks.today)
        net.cost <- input$leg1.cost*input$leg1.contracts + input$leg2.cost*input$leg2.contracts
        spread.net.stats <- data.frame(Net.Cost=net.cost*100+abs(input$shares*input$share.price),Net.Delta=sum(spread.greeks.today$Delta)+input$shares, Net.Gamma=sum(spread.greeks.today$Gamma), Net.Theta=sum(spread.greeks.today$Theta), Net.Vega=sum(spread.greeks.today$Vega))
        spread.net.stats
    })

    output$Instructions <- renderText({
        
        a <- as.character("Enter the available option prices manually for 1-2 legs.
 Make sure call/put flags and contract #s are accurate.
 Set the volatility slider around the est. IV %, or until the thick, curvy line shows 0 P/L at the spot price.
 Observe the lines and theoretical greeks. Between today (Tn) and Expiry (T0), there are 3 P/L curves:
 The most rigid line is Expiry. The thin line is halfway until expiry. Other line is today's curve.
 Adjust volatility to observe how your spread is affected by changes in volatility. Check out the net greeks.
                          
                          ")
        a
    })

    
    
    
})

# input$leg1.cost <- 3.50
# input$leg1.flag <- 'c'
# input$spot <- 100
# input$leg1.strike <- 102
# t <- 300


#     d <- ggplot(xaxis, pnl3, xlab="Underlying Stock Price", ylab = "P/L") + geom_point(aes(y=pnl, color=pnl), alpha=.8) +
#         geom_line(aes(y=pnl2, color=pnl2), alpha=.9) + 
#         geom_point(aes(y=pnl3, color=pnl3)) + 
#         scale_colour_gradient2(limits=c(min(pnl3,pnl2,pnl),max(pnl,pnl2,pnl3)), low="red", mid="white", high=muted("green")) + 
#         theme(panel.background = element_rect(fill = 'lightblue')) + geom_hline(aes(y=0), alpha=.4)
#     