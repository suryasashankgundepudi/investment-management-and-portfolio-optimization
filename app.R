#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(ggfortify)
library(ggthemes)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)



# Define UI for application that gives the risk profile, portfolio and correlation
ui <- fluidPage(


    
    #fluid page scales the components to fit on all browsers
    
    #Header panel 
    headerPanel(h1("Portfolio Optimization Based on Historical Data",
                   style="color:black"),
                
                list(tags$head(tags$style("body {background-color: #ADD8E6 ;}")))),
    
    #layout
    sidebarLayout(
        
        # Input sidebar layout
        sidebarPanel(
            
            # Getting Stock values from user
            textInput("ticker","Enter the tickers of desired stocks", 
                      value = toupper("MSFT GOOG IBM")),
            
            submitButton("Submit"),
            
            p("Kindly seperate stock tickers with space")
        ),
        mainPanel(#poistion="right",
            
            tabsetPanel(type="tab",
                        
                        tabPanel("Risk over time",
                                 
                                 
                                 plotOutput("p1"),
                                 
                                 h3("Statistics"),
                                 p("Skewness"),
                                 verbatimTextOutput("skew"),
                                 p("Kurtosis"),
                                 verbatimTextOutput("kurt"),
                                 p("Value at Risk"),
                                 verbatimTextOutput("var"),
                               
                        ),
                        
                        tabPanel("Suggested Portfolio based on past data",
                                 h2("Minimimum variance potfolio"),         
                                 plotOutput("p2"), 
                                 h4("Optimised weights for the given stocks in percenatges are"),
                                 verbatimTextOutput("wt")         
                                 
                                 
                        ),
                        tabPanel("Correlation",
                                 h4("Correlation among selected Stocks"),
                                 plotOutput("p3")
                        ),
                        tabPanel("Prices for each stock, visualized",
                                 verbatimTextOutput("tab"),
                                 plotOutput("p4")
                        )
            )
        )
        
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input,output){
    
    #getting returns for the stocks 
    df <- reactive({
        
        # Using text input seperated by space for ticker valuje
        ticker <- unlist(strsplit(x = input$ticker, split =  " "))
        
        
        # Gets the stocks with only past 10 years data
        Stocks = lapply(ticker, function(tickVal) {
            
            # Removing the missing values for stocks without 10 year data
            na.omit(getSymbols(tickVal,auto.assign=FALSE,src="yahoo")[,4])
            
        })
        
        
        # Calling back the stocks and removing the missing values
        x<-do.call(cbind, Stocks)
        df<-x[complete.cases(x), ]
        
        # Changing the names of columns in DF
        for(name in names(df)){
            colnames(df)[colnames(df)==name] <- strsplit(name,"\\.")[[1]][1]}
        
        # New dataframe will have the closing prices of these stocks
        return(df)
        
        
    })
    
    returns<-reactive({
        return(Return.calculate(df())[-1])
        
    })
    
    
    opt<-reactive({
        
        # Create the portfolio 
        port_spec <- portfolio.spec(colnames(returns()))
        
        # Making Sure that the portfolio is constrained to sum to one
        port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
        
        # This makes sure that the weight of each portfolio is between 0 and 1
        # This means that you're only holding and not going short
        port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
        
        # We will now optimize portfolio by minimizing standard deviation of risk
        port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
        
        # solve the optimization problem of the portfolio using return of investment
        opt <- optimize.portfolio(returns(), portfolio = port_spec, 
                                  optimize_method = "ROI")
        
        return(opt)
    })
    
    
    wts<-reactive({
    ``    
        return(extractWeights(opt()))
        
    })
    

    
    #getting output using desired functions 
    
    # Plotting the risk over past 10 years
    output$p1 <- renderPlot(autoplot(returns(), xlab = "Year", 
                                     ts.colour = '#3154DC')+theme_hc())
    
    
    
    # getting the skewness of the returns of each stock 
    output$skew <- renderPrint(round(skewness(returns()), 3))
    
    # getting the kurtosis of each stock
    output$kurt <- renderPrint(round(kurtosis(returns()), 3))
    
    # Getting the variance of each stock over past few years
    output$var <- renderPrint(round(VaR(returns()), 3)) 
    
    
    # This returns the percentage weights of the stocks 
    output$wt<-renderPrint(round(wts()*100,2))
    
    
    # Returns a bar plot of the weights 
    output$p2<-renderPlot(barplot(wts(),
                                  col="#3D9970",
                                  xlab = "Stock Names", 
                                  ylab="Weights of Values", 
                                  main ="OPtimized Weight distribution") + 
                              grid())
    
    # This plots a nXn subplot with the correlations among each security
    output$p3<-renderPlot(chart.Correlation(returns()))
    
    
    # Returning the table for annualizer returns
    output$tab<-renderPrint(table.AnnualizedReturns(returns()))
    
    # Plotting a time series plot of the prices of the each stock
    output$p4<-renderPlot(chart.TimeSeries(df(),main="Prices",legend.loc="topleft")
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
