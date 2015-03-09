# How to import 1min, 5min, 15min, price data using Tradier's free API
# Michael Julian

# Load Libraries - if this is your first time running R, you must 
# include install.packages(c('RCurl','XML'))
library(RCurl)
library(XML)

# YOU MUST GET YOUR OWN AUTH TOKEN
# Visit developer.tradier.com to get a free dev account

# FUNCTION FORM:
importPriceData <- function(symbol,interval,auth, days){
    
    xmlOption <- curlOptions(httpheader=c(Authorization=auth, 
                                          Accept="application/xml"))

    # start == 1 week ago at open
    start=paste0(Sys.Date()-days, 'T09:30')
    
    # end == today in YY-MM-DDTHH:MM 24 hr format
    end=paste0(Sys.Date(),'T',format(Sys.time(),format='%H:%M'))
    
    # import as XML 
    xmlData <- getURL(
        paste0('https://sandbox.tradier.com/v1/markets/timesales?symbol=',symbol,
               '&interval=', interval, '&start=', start,
               '&end=', end),
        .opts=xmlOption)
    
    df <- xmlToDataFrame(xmlData)
    df$time <- gsub('T', ' ', df$time)
    df
}

# Replace 'auth' with your own auth token
test.data.frame <- importPriceData(symbol='aapl',
                        interval='1min',
                        auth='Bearer YOUR_AUTH_TOKEN', 7)

head(test.data.frame)