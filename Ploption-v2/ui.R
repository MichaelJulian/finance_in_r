
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
# library(rCharts)
library(quantmod)
library(ggplot2)
require(ggplot2)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Ploption: A P&L tool for Plotting Options"),
    sidebarLayout(position = "left",
                  sidebarPanel(
                      helpText(h4("Enter parameters below :")),
                      
                      numericInput("spot", 
                                   "Stock Price",
                                   value=100),
                      dateRangeInput("dates", label = ("Date range"), start = as.character(Sys.Date()), end = as.character(Sys.Date()+60)),
                      numericInput("rf", 
                                   label = "Risk Free Rate (Enter as decimal)",
                                   value=.01, step=.001),
                      numericInput("div", 
                                   label = "Dividend yield (Enter as decimal)",
                                   value=.00, step=.001),
                      submitButton("Update View"),
                      tags$head(
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 10px} ")),
                      
                      tabsetPanel(
                            tabPanel(
                                title="Leg 1",
                                textOutput("leg1.iv_estimate"),
                                sliderInput("vol.1", label = "", min =0, max =50, value = 15, step=.1),
                                numericInput("leg1.strike", 
                                   label = "Strike Price",
                                   value=100),
                                numericInput("leg1.cost", 
                                    label ="Premium",
                                    value=2.50, step=0.01),
                                
                                numericInput("leg1.contracts", 
                                    label = "# Contracts (negative if short)",
                                    value=1),
                                radioButtons('leg1.flag', "Call / Put:", c("Call"='call', "Put"='put'), inline=TRUE)),
                            tabPanel(
                                title = "Leg 2",
                                textOutput("leg2.iv_estimate"),
                                sliderInput("vol.2", label = "", min =0, max =50, value = 15, step=.1),
                                numericInput("leg2.strike", 
                                             label = "Strike Price",
                                             value=100),
                                
                                numericInput("leg2.cost", 
                                             label ="Premium",
                                             value=0.00),
                                numericInput("leg2.contracts", 
                                             label = "# Contracts (negative if short)",
                                             value=0),
                                radioButtons('leg2.flag', "Call / Put:", c("Call"='call', "Put"='put'), inline=TRUE)
                                
                                ),
                            tabPanel(
                                title = "Shares",
                                numericInput("shares", 
                                             label = "# Shares",
                                             value=0),
                                
                                numericInput("share.price", 
                                             label ="Price",
                                             value=100, step=.01)
                                
                            )
                            
                                        

                                
                                )),
#     sidebarLayout(position = 'right',
#                    sidebarPanel(
#                        titlePanel(h6("Option Leg 2")),
#                        )),
    
    # Show a plot of the generated distribution
    mainPanel(
            plotOutput('pnlChart_gg'),
            h5("Theoretical Greeks"),
            tableOutput('leg_greeks'),
            tableOutput('net_greeks'),
            h5("Instructions"),
            textOutput('Instructions')
    )
)))

