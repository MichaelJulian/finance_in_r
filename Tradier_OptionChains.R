# This code utilizes JSON, Curl, and XML to pull option data from 
# Tradier's developer sandbox. 
library(RCurl)
library(httr)
library(XML)
library(jsonio)

# To obtain a free authorization token, visit https://developer.tradier.com/

pullOptionChain <- function(symbol, monthNum){
    # Set Curl options, one for xml and one for json
    jsonOption <- curlOptions(httpheader=c(Authorization='Bearer {auth token here}', 
                                           Accept="application/json"))
    xmlOption <- curlOptions(httpheader=c(Authorization='Bearer {auth token here}', 
                                          Accept="application/xml"))
    
    
    # First, Find Expirations for symbol SPY
    exp <-getURL(paste0('https://sandbox.tradier.com/v1/markets/options/expirations?symbol=',symbol), 
                 .opts=jsonOption)
    doc <- fromJSON(exp)
    weeklyExpiry <- doc[[1]][[1]][monthNum]
    
    # Second, use XML options to pull option data from symbol
    y <-getURL(
        paste0('https://sandbox.tradier.com/v1/markets/options/chains?symbol=',
               symbol,'&expiration=',
               weeklyExpiry), 
        .opts=xmlOption)
    
    df <- xmlToDataFrame(y)
    
    # remove some of the unimportant columns
    chain <- df[,c(1,8,12,13,14,15,16,19,20,23,24,28,29,31,32,33)]
    chain
}


