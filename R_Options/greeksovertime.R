# This code
# - calculates greeks over time for ATM, 1SD, 2SD calls 
# - plots the greeks for each call
# Author: Michael Julian


install.packages('fOptions','ggplot2')

##### if you do not have fOptions/ggplot2, run install.packages() first.
library(fOptions); library(ggplot2)

# OTM CALL DELTA OVER TIME
sd_2_delta_call = GBSGreeks('delta', 'c', S=100, X=116 , Time=(1:60)/365, 
          r=.02, b=0, sigma=.35)
sd_1_delta_call = GBSGreeks('delta', 'c', S=100, X=108 , Time=(1:60)/365, 
          r=.02, b=0, sigma=.35)
sd_0_delta_call = GBSGreeks('delta', 'c', S=100, X=100 , Time=(1:60)/365, 
          r=.02, b=0, sigma=.35)


# ATM/OTM CALL THETA OVER TIME
sd_2_theta_call = GBSGreeks('theta', 'c', S=100, X=116 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_1_theta_call = GBSGreeks('theta', 'c', S=100, X=108 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_0_theta_call = GBSGreeks('theta', 'c', S=100, X=100 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)

# ATM/OTM CALL GAMMA OVER TIME
sd_2_gamma_call = GBSGreeks('gamma', 'c', S=100, X=116 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_1_gamma_call = GBSGreeks('gamma', 'c', S=100, X=108 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_0_gamma_call = GBSGreeks('gamma', 'c', S=100, X=100 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)

# ATM/OTM CALL VEGA OVER TIME
sd_2_vega_call = GBSGreeks('vega', 'c', S=100, X=116 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_1_vega_call = GBSGreeks('vega', 'c', S=100, X=108 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)
sd_0_vega_call = GBSGreeks('vega', 'c', S=100, X=100 , Time=(1:60)/365, 
                            r=.02, b=0, sigma=.35)

df = data.frame(Option = factor(c(rep('ATM Call',60), rep('1 SD Call', 60), rep('2 SD Call', 60))),
                Dte= c(rep(1:60,3)),
    Gamma=c(sd_0_gamma_call, sd_1_gamma_call, sd_2_gamma_call),
    Delta=c(sd_0_delta_call, sd_1_delta_call, sd_2_delta_call),
    Theta=c(sd_0_theta_call, sd_1_theta_call, sd_2_theta_call),
    Vega =c(sd_0_vega_call, sd_1_vega_call, sd_2_vega_call))

# GAMMA PLOT
ggplot(df) + geom_line(aes(x=Dte,y=Gamma, colour=Option)) + 
    geom_point(aes(x=Dte,y=Gamma, colour=Option)) + 
    ggtitle('GAMMA OVER TIME: (Spot = 100) (IV = 35) (Rf = 1%)')

# DELTA PLOT
ggplot(df) + geom_line(aes(x=Dte,y=Delta, colour=Option)) + 
    geom_point(aes(x=Dte,y=Delta, colour=Option)) + 
    ggtitle('DELTA OVER TIME: (Spot = 100) (IV = 35) (Rf = 1%)')

# THETA PLOT
ggplot(df) + geom_line(aes(x=Dte,y=Theta, colour=Option)) + 
    geom_point(aes(x=Dte,y=Theta, colour=Option)) + 
    ggtitle('THETA OVER TIME: (Spot = 100) (IV = 35) (Rf = 1%)')

# VEGA PLOT
ggplot(df) + geom_line(aes(x=Dte,y=Vega, colour=Option)) + 
    geom_point(aes(x=Dte,y=Vega, colour=Option)) + 
    ggtitle('VEGA OVER TIME: (Spot = 100) (IV = 35) (Rf = 1%)')
