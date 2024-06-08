library(shiny)
library(ggthemes)  # For additional themes if used in plots
library(ggfortify)
# Define the user interface
ui <- fluidPage(
  # Fluid page scales the components to fit on all browsers
  tags$head(tags$style(HTML("body {background-color: #ADD8E6;}"))),  # Styling the background color
  
  # Header Panel
  headerPanel(
    h1("Portfolio Optimization Based on Historical Data", style = "color:black;")
  ),
  
  # Sidebar Layout
  sidebarLayout(
    # Input Sidebar
    sidebarPanel(
      # Input for Stock Tickers
      textInput("ticker", "Enter the tickers of desired stocks", value = "MSFT GOOG IBM"),
      submitButton("Submit"),
      p("Kindly separate stock tickers with space.")
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Tab for Risk Analysis
                  tabPanel("Risk over past 10 years",
                           plotOutput("p1"),
                           h3("Statistics"),
                           p("Skewness"),
                           verbatimTextOutput("skew"),
                           p("Kurtosis"),
                           verbatimTextOutput("kurt"),
                           p("Value at Risk"),
                           verbatimTextOutput("var")
                  ),
                  
                  # Tab for Portfolio Suggestions
                  tabPanel("Suggested Portfolio based on past data",
                           h2("Minimum Variance Portfolio"),
                           plotOutput("p2"), 
                           h4("Optimized weights for the given stocks in percentages are"),
                           verbatimTextOutput("wt")
                  ),
                  
                  # Tab for Correlation Analysis
                  tabPanel("Correlation",
                           h4("Correlation among selected Stocks"),
                           plotOutput("p3")
                  ),
                  
                  # Tab for Stock Prices Visualization
                  tabPanel("Prices for each stock, visualized",
                           verbatimTextOutput("tab"),
                           plotOutput("p4")
                  )
      )
    )
  )
)

# End of UI definition
