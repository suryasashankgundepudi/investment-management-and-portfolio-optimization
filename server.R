library(ggfortify)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# Extract stock data based on user input
get_stock_data <- function(ticker_input, start_date, end_date) {
  tickers <- unlist(strsplit(ticker_input, " "))
  stock_data <- lapply(tickers, function(ticker) {
    tryCatch({
      na.omit(getSymbols(ticker, auto.assign = FALSE, src = "yahoo", from = start_date, to = end_date)[,4])
    }, error = function(e) NULL)
  })
  do.call(cbind, stock_data)
}


# Calculate returns
calculate_returns <- function(stock_data) {
  stock_data <- stock_data[complete.cases(stock_data), ]
  Return.calculate(stock_data)[-1]
}

# Optimize portfolio
optimize_portfolio <- function(returns) {
  port_spec <- portfolio.spec(colnames(returns))
  port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
  port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
  optimize.portfolio(returns, portfolio = port_spec, optimize_method = "ROI")
}

# Server function defining how outputs are calculated from inputs
server <- function(input, output) {
  
  # Reactive expression for cleaned and merged stock data
  stock_data <- reactive({
    req(input$ticker, input$startDate, input$endDate)  # Ensure that the ticker and date inputs are available
    get_stock_data(input$ticker, input$startDate, input$endDate)
  })
  
  # Reactive expression for stock returns
  returns <- reactive({
    req(stock_data())  # Ensure that stock data is available
    calculate_returns(stock_data())
  })
  
  # Reactive expression for optimized portfolio
  optimized_portfolio <- reactive({
    req(returns())  # Ensure that returns data is available
    optimize_portfolio(returns())
  })
  
  # Output for the risk plot
  output$p1 <- renderPlot({
    req(returns())  # Ensure that returns data is available
    autoplot(returns(), xlab = "Year", ts.colour = '#3154DC') + theme_hc()
  })
  
  # Outputs for skewness, kurtosis, and VaR
  output$skew <- renderPrint({ round(skewness(returns()), 3) })
  output$kurt <- renderPrint({ round(kurtosis(returns()), 3) })
  output$var <- renderPrint({ round(VaR(returns()), 3) })
  
  # Output for optimized weights
  output$wt <- renderPrint({
    weights <- extractWeights(optimized_portfolio())
    round(weights * 100, 2)
  })
  
  # Output for the optimized portfolio plot
  output$p2 <- renderPlot({
    weights <- extractWeights(optimized_portfolio())
    barplot(weights, col="#3D9970", xlab = "Stock Names", ylab="Weights in %", main ="Optimized Weight Distribution") +
      grid()
  })
  
  # Output for correlation plot
  output$p3 <- renderPlot({
    req(returns())  # Ensure that returns data is available
    chart.Correlation(returns())
  })
  
  # Output for annualized returns table
  output$tab <- renderPrint({
    req(returns())  # Ensure that returns data is available
    table.AnnualizedReturns(returns())
  })
  
  # Output for time series plot of stock prices
  output$p4 <- renderPlot({
    req(stock_data())  # Ensure that stock data is available
    chart.TimeSeries(stock_data(), main="Prices", legend.loc="topleft")
  })
}
